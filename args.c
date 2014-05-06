/*
  convenient routines for argument list handling

  the args list always contains argc+1 items.
 */

#include "f90cache.h"

ARGS *args_init(int init_argc, char **init_args)
{
    ARGS *args;
    int i;
    args = (ARGS *)x_malloc(sizeof(ARGS));
    args->argc = 0;
    args->argv = (char **)x_malloc(sizeof(char *));
    args->argv[0] = NULL;
    for (i=0;i<init_argc;i++) {
	args_add(args, init_args[i]);
    }
    return args;
}

/* remove all args off the args list */
void args_clean(ARGS *args)
{
    while (args->argc--) {
//printf(" (args_clean:) freeing arg # %d\n",args->argc);
//printf("               addr of this: 0x%x\n",(unsigned int)args->argv[args->argc]);
	free(args->argv[args->argc]);
    }
    free(args->argv);
    free(args);
}

void args_add(ARGS *args, const char *s)
{
    args->argv = (char**)realloc(args->argv, (args->argc + 2) * sizeof(char *));
    args->argv[args->argc] = x_strdup(s);
    args->argc++;
    args->argv[args->argc] = NULL;
}

/* pop n last element(s) off the args list */
void args_pop(ARGS *args, int n)
{
    while (n--) {
	args->argc--;
	free(args->argv[args->argc]);
	args->argv[args->argc] = NULL;
    }
}

/* remove the first element of the argument list */
void args_remove_first(ARGS *args)
{
    free(args->argv[0]);
    memmove(&args->argv[0], &args->argv[1],
	    args->argc * sizeof(char *));
    args->argc--;
}

/* add an argument into the front of the argument list */
void args_add_prefix(ARGS *args, const char *s)
{
    args->argv = (char**)realloc(args->argv, (args->argc + 2) * sizeof(char *));
    args->argc++;
    memmove(&args->argv[1], &args->argv[0],
	    args->argc * sizeof(char *));
    args->argv[0] = x_strdup(s);
}

/* process list when founding a specified prefix -- remove 1 arg */
void args_strip(ARGS *args, const char *prefix)
{
    int i;
    for (i=0; i<args->argc; ) {
	if (strncmp(args->argv[i], prefix, strlen(prefix)) == 0) {
	    free(args->argv[i]);
	    memmove(&args->argv[i], &args->argv[i+1],
		    (args->argc - i) * sizeof(char *));
	    args->argc--;
	} else {
	    i++;
	}
    }
}

/* process list when founding a specified prefix -- remove 2 args */
void args_strip_2(ARGS *args, const char *prefix)
{
    int i;
    for (i=0; i<args->argc; ) {
	if (strncmp(args->argv[i], prefix, strlen(prefix)) == 0) {
	    free(args->argv[i]);
	    free(args->argv[i+1]);
	    memmove(&args->argv[i], &args->argv[i+2],
		    (args->argc - i - 1) * sizeof(char *));
	    args->argc = args->argc - 2;
	} else {
	    i++;
	}
    }
}

