/*
  Copyright (C) 2016 by Syohei YOSHIDA

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#define _BSD_SOURCE
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <emacs-module.h>

#include <sqlite3.h>

int plugin_is_GPL_compatible;

struct el_sql_resultset {
	sqlite3 *db;
	sqlite3_stmt *stmt;
	emacs_value fields;
	bool eof;
};

static char*
retrieve_string(emacs_env *env, emacs_value str, ptrdiff_t *size)
{
	*size = 0;

	env->copy_string_contents(env, str, NULL, size);
	char *p = malloc(*size);
	if (p == NULL) {
		*size = 0;
		return NULL;
	}
	env->copy_string_contents(env, str, p, size);

	return p;
}

static void
el_sqlite3_free(void *arg)
{
	sqlite3_close((sqlite3*)arg);
}

static void
el_sqlite3_resultset_free(void *arg)
{
	struct el_sql_resultset *rs = (struct el_sql_resultset*)arg;
	sqlite3_finalize(rs->stmt);
	free(rs);
}

static emacs_value
Fsqlite3_new(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	emacs_value db_path = args[0];
	char *name = NULL;

	if (env->is_not_nil(env, db_path)) {
		ptrdiff_t size;
		name = retrieve_string(env, db_path, &size);
	}

	sqlite3 *sdb;
	int ret = sqlite3_open_v2(name ? name : ":memory:", &sdb,
				  SQLITE_OPEN_FULLMUTEX
				  | SQLITE_OPEN_READWRITE
				  | SQLITE_OPEN_CREATE
#ifdef SQLITE_OPEN_URI
				  | SQLITE_OPEN_URI
#endif
				  | 0, NULL);
	if (ret != SQLITE_OK) {
		free(name);
		return env->intern(env, "nil");
	}

	free(name);
	return env->make_user_ptr(env, el_sqlite3_free, sdb);
}

static bool
eq_type(emacs_env *env, emacs_value type, const char *type_str)
{
	return env->eq(env, type, env->intern(env, type_str));
}

static const char*
bind_values(emacs_env *env, sqlite3 *db, sqlite3_stmt *stmt, emacs_value bounds)
{
	sqlite3_reset(stmt);
	int len = (int)env->vec_size(env, bounds);

	for (int i = 0; i < len; ++i) {
		int ret = SQLITE_MISMATCH;
		emacs_value bound = env->vec_get(env, bounds, i);
		emacs_value type = env->type_of(env, bound);

		if (eq_type(env, type, "string")) {
			ptrdiff_t size;
			const char *p = retrieve_string(env, bound, &size);
			ret = sqlite3_bind_text(stmt, i+1, p, size-1, NULL);
		} else if (eq_type(env, type, "integer")) {
			intmax_t num = env->extract_integer(env, bound);
			ret = sqlite3_bind_int64(stmt, i+1, num);
		} else if (eq_type(env, type, "float")) {
			double num = env->extract_float(env, bound);
			ret = sqlite3_bind_double(stmt, i+1, num);
		} else if (env->eq(env, bound, env->intern(env, "nil"))) {
			ret = sqlite3_bind_null(stmt, i+1);
		} else if (env->eq(env, bound, env->intern(env, "true"))) {
			ret = sqlite3_bind_int(stmt, i+1, 1);
		} else if (env->eq(env, bound, env->intern(env, "false"))) {
			ret = sqlite3_bind_int(stmt, i+1, 0);
		} else {
			return "invalid argument";
		}

		if (ret != SQLITE_OK)
			return sqlite3_errmsg(db);
	}

	return NULL;
}

static emacs_value
Fsqlite3_execute_batch(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	sqlite3 *sdb = env->get_user_ptr(env, args[0]);
	ptrdiff_t size;
	char *query = retrieve_string(env, args[1], &size), *sql, *tail;
	emacs_value Qnil = env->intern(env, "nil");
	emacs_value retval = Qnil;
	const char *errmsg = NULL;

	char *top = malloc(size);
	if (top == NULL) {
		free(query);
		return Qnil;
	}

	memcpy(top, query, size);
	tail = top;

	while (*(sql = tail)) {
		sqlite3_stmt *stmt = NULL;
		int ret = sqlite3_prepare_v2(sdb, sql, -1, &stmt, (const char**)&tail);
		if (nargs > 2) {
			const char *err = bind_values(env, sdb, stmt, args[2]);
			if (err != NULL) {
				errmsg = err;
				goto exit;
			}
		}

		if (ret != SQLITE_OK) {
			if (stmt != NULL) {
				sqlite3_finalize(stmt);
				sqlite3_reset(stmt);
			}

			errmsg = sqlite3_errmsg(sdb);
			goto exit;
		}

		if (stmt == NULL)
			continue;

		ret = sqlite3_step(stmt);
		sqlite3_finalize(stmt);
		if (ret != SQLITE_OK && ret != SQLITE_DONE) {
			errmsg = sqlite3_errmsg(sdb);
			goto exit;
		}
	}

	retval = env->make_integer(env, sqlite3_changes(sdb));

exit:
	free(top);
	free(query);

	if (errmsg != NULL) {
		emacs_value errstr = env->make_string(env, errmsg, strlen(errmsg));
		env->non_local_exit_signal(env, env->intern(env, "error"), errstr);
	}

	return retval;
}

static emacs_value
row_to_value(emacs_env *env, sqlite3_stmt *stmt)
{
	int len = sqlite3_column_count(stmt);
	emacs_value values = env->intern(env, "nil");
	emacs_value Qcons = env->intern(env, "cons");
	emacs_value args[2];

	for (int i = 0; i < len; ++i) {
		emacs_value v;

		switch (sqlite3_column_type(stmt, i)) {
		case SQLITE_INTEGER: {
			sqlite3_int64 value = sqlite3_column_int64(stmt, i);
			v = env->make_integer(env, value);
			break;
		}
		case SQLITE_FLOAT: {
			double value = sqlite3_column_int64(stmt, i);
			v = env->make_integer(env, value);
			break;
		}
		case SQLITE_BLOB: {
			int size = sqlite3_column_bytes(stmt, i);
			const char* ptr = sqlite3_column_blob(stmt, i);
			v = env->make_string(env, ptr, size);
			break;
		}
		case SQLITE_NULL:
			v = env->intern(env, "nil");
			break;
		case SQLITE_TEXT: {
			const char *text = (const char*)sqlite3_column_text(stmt, i);
			v = env->make_string(env, text, strlen(text));
			break;
		}
		}

		args[0] = v;
		args[1] = values;
		values = env->funcall(env, Qcons, 2, args);
	}

	emacs_value rargs[] = {values};
	return env->funcall(env, env->intern(env, "reverse"), 1, rargs);
}

static emacs_value
Fsqlite3_execute(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	sqlite3 *sdb = env->get_user_ptr(env, args[0]);
	ptrdiff_t size;
	char *query = retrieve_string(env, args[1], &size);
	emacs_value Qnil = env->intern(env, "nil");
	emacs_value retval = Qnil;
	const char *errmsg = NULL;

	sqlite3_stmt *stmt = NULL;
	int ret = sqlite3_prepare_v2(sdb, query, size, &stmt, NULL);
	if (ret != SQLITE_OK) {
		if (stmt) {
			sqlite3_finalize(stmt);
		}

		goto exit;
	}

	emacs_value Qcons = env->intern(env, "cons");
	emacs_value fields = Qnil;
	int count = sqlite3_column_count(stmt);
	for (int i = 0; i < count; ++i) {
		const char *name = sqlite3_column_name(stmt, i);
		emacs_value cargs[] = {
			env->make_string(env, name, strlen(name)),
			fields,
		};
		fields = env->funcall(env, Qcons, 2, cargs);
	}

	emacs_value Qreverse = env->intern(env, "reverse");
	emacs_value rargs[] = {fields};
	fields = env->funcall(env, Qreverse, 1, rargs);

	emacs_value cb = args[2];
	if (!env->is_not_nil(env, cb)) {
		struct el_sql_resultset *result = malloc(sizeof(struct el_sql_resultset));

		result->db = sdb;
		result->stmt = stmt;
		result->fields = fields;
		result->eof = false;
		retval = env->make_user_ptr(env, el_sqlite3_resultset_free, result);
		goto exit;
	}

	while ((ret = sqlite3_step(stmt)) == SQLITE_ROW) {
		emacs_value cb_args[2];
		cb_args[0] = row_to_value(env, stmt);
		cb_args[1] = fields;
		env->funcall(env, cb, 2, cb_args);
	}

	sqlite3_finalize(stmt);

 exit:
	free(query);

	if (errmsg != NULL) {
		emacs_value errstr = env->make_string(env, errmsg, strlen(errmsg));
		env->non_local_exit_signal(env, env->intern(env, "error"), errstr);
	}

	return retval;
}

static emacs_value
el_sqlite3_exec(emacs_env *env, sqlite3 *sdb, const char *query)
{
	int ret = sqlite3_exec(sdb, query, NULL, NULL, NULL);
	if (ret != SQLITE_OK) {
		return env->intern(env, "nil");
	}

	return env->intern(env, "t");
}

static emacs_value
Fsqlite3_transaction(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	sqlite3 *sdb = env->get_user_ptr(env, args[0]);
	return el_sqlite3_exec(env, sdb, "begin");
}

static emacs_value
Fsqlite3_commit(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	sqlite3 *sdb = env->get_user_ptr(env, args[0]);
	return el_sqlite3_exec(env, sdb, "commit");
}

static emacs_value
Fsqlite3_rollback(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	sqlite3 *sdb = env->get_user_ptr(env, args[0]);
	return el_sqlite3_exec(env, sdb, "rollback");
}

static emacs_value
Fsqlite3_resultset_next(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	struct el_sql_resultset *result = env->get_user_ptr(env, args[0]);

	int ret = sqlite3_step(result->stmt);
	if (ret != SQLITE_ROW && ret != SQLITE_OK && ret != SQLITE_DONE) {
		const char *errmsg = sqlite3_errmsg(result->db);
		env->non_local_exit_signal(env, env->intern(env, "error"),
					   env->make_string(env, errmsg, strlen(errmsg)));
		return env->intern(env, "nil");
	}

	if (ret == SQLITE_DONE) {
		result->eof = true;
		return env->intern(env, "nil");
	}

	return row_to_value(env, result->stmt);
}

static emacs_value
Fsqlite3_resultset_fields(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	struct el_sql_resultset *result = env->get_user_ptr(env, args[0]);
	return result->fields;
}

static emacs_value
Fsqlite3_resultset_eof(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
	struct el_sql_resultset *result = env->get_user_ptr(env, args[0]);
	if (result->eof) {
		return env->intern(env, "t");
	} else {
		return env->intern(env, "nil");
	}
}

static void
bind_function(emacs_env *env, const char *name, emacs_value Sfun)
{
	emacs_value Qfset = env->intern(env, "fset");
	emacs_value Qsym = env->intern(env, name);
	emacs_value args[] = { Qsym, Sfun };

	env->funcall(env, Qfset, 2, args);
}

static void
provide(emacs_env *env, const char *feature)
{
	emacs_value Qfeat = env->intern(env, feature);
	emacs_value Qprovide = env->intern (env, "provide");
	emacs_value args[] = { Qfeat };

	env->funcall(env, Qprovide, 1, args);
}

int
emacs_module_init(struct emacs_runtime *ert)
{
	emacs_env *env = ert->get_environment(ert);

#define DEFUN(lsym, csym, amin, amax, doc, data) \
	bind_function (env, lsym, env->make_function(env, amin, amax, csym, doc, data))

	DEFUN("sqlite3-core-new", Fsqlite3_new, 1, 1, NULL, NULL);
	DEFUN("sqlite3-core-execute-batch", Fsqlite3_execute_batch, 2, 3, NULL, NULL);
	DEFUN("sqlite3-core-execute", Fsqlite3_execute, 3, 3, NULL, NULL);
	DEFUN("sqlite3-transaction", Fsqlite3_transaction, 1, 1, NULL, NULL);
	DEFUN("sqlite3-commit", Fsqlite3_commit, 1, 1, NULL, NULL);
	DEFUN("sqlite3-rollback", Fsqlite3_rollback, 1, 1, NULL, NULL);

	// resultset API
	DEFUN("sqlite3-resultset-next", Fsqlite3_resultset_next, 1, 1, NULL, NULL);
	DEFUN("sqlite3-resultset-fields", Fsqlite3_resultset_fields, 1, 1, NULL, NULL);
	DEFUN("sqlite3-resultset-eof", Fsqlite3_resultset_eof, 1, 1, NULL, NULL);

#undef DEFUN

	provide(env, "sqlite3-core");
	return 0;
}

/*
  Local Variables:
  c-basic-offset: 8
  indent-tabs-mode: t
  End:
*/
