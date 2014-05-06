#include "f90cache.h"

/*
  execute a compiler backend, capturing all output to the given paths;
  the full path to the compiler to run is in argv[0]
*/
int execute(char **argv,
	    const char *path_stdout,
	    const char *path_stderr)
{
    pid_t pid;
    int status;

    pid = fork();
    if (pid == -1) fatal("Failed to fork");

    if (pid == 0) {
	int fd;

	unlink(path_stdout);
	fd = open(path_stdout, O_WRONLY|O_CREAT|O_TRUNC|O_EXCL|O_BINARY, 0666);
	if (fd == -1) {
	    exit(STATUS_NOCACHE);
	}
	dup2(fd, 1);
	close(fd);

	unlink(path_stderr);
	fd = open(path_stderr, O_WRONLY|O_CREAT|O_TRUNC|O_EXCL|O_BINARY, 0666);
	if (fd == -1) {
	    exit(STATUS_NOCACHE);
	}
	dup2(fd, 2);
	close(fd);

	exit(execv(argv[0], argv));
    }

    if (waitpid(pid, &status, 0) != pid) {
	fatal("waitpid failed");
    }

    if (WEXITSTATUS(status) == 0 && WIFSIGNALED(status)) {
	return -1;
    }

    return WEXITSTATUS(status);
}

/*
  find an executable by name in $PATH. Exclude any that are links
  to exclude_name
*/
char *find_executable(const char *name, const char *exclude_name)
{
    char *path;
    char *tok;
    struct stat st1, st2;

    if (*name == '/') {
	return x_strdup(name);
    }

    path = getenv("F90CACHE_PATH");
    if (!path) {
	path = getenv("PATH");
    }
    if (!path) {
	fc_log("no PATH variable!?\n");
	return NULL;
    }

    path = x_strdup(path);

    /* search the path looking for the first compiler of the right name
       that isn't us */
    for (tok=strtok(path,":"); tok; tok = strtok(NULL, ":")) {
	char *fname;
	x_asprintf(&fname, "%s/%s", tok, name);
	/* look for a normal executable file */
	if (access(fname, X_OK) == 0 &&
	     lstat(fname, &st1) == 0 &&
	     stat(fname, &st2) == 0 &&
	     S_ISREG(st2.st_mode)) {
	    /* if its a symlink then ensure it doesn't
		point at something called exclude_name */
	    if (S_ISLNK(st1.st_mode)) {
		char *buf = x_realpath(fname);
		if (buf) {
		    char *p = str_basename(buf);
		    if (strcmp(p, exclude_name) == 0) {
			/* its a link to "f90cache" ! */
			free(p);
			free(buf);
			continue;
		    }
		    free(buf);
		    free(p);
		}
	    }

	    /* found it! */
	    free(path);
	    return fname;
	}
	free(fname);
    }

    return NULL;
}
