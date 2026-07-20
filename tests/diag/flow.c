/* Positive cases for unreachable-code and missing-return.
 *
 * The negative cases matter more: every function below named ok_* is a shape
 * that must NOT warn, and they are the shapes that produced 118 false
 * positives on a real codebase before the switch and noreturn handling.
 */

extern void exit(int);

/* WARNS: statement after a return */
int dead_after_return(void) {
    return 1;
    return 2;
}

/* WARNS: falls off the end of a non-void function */
int no_return_on_one_path(int c) {
    if (c) {
        return 1;
    }
}

/* ok: every case returns and there is a default */
int ok_switch_all_return(int c) {
    switch (c) {
    case 1:
        return 10;
    case 2:
        return 20;
    default:
        return 0;
    }
}

/* ok: both arms return */
int ok_if_else(int c) {
    if (c) {
        return 1;
    } else {
        return 2;
    }
}

/* ok: an infinite loop with no break never reaches the end */
int ok_forever(void) {
    for (;;) {
    }
}

/* ok: ends in a call that does not come back */
int ok_noreturn_tail(int c) {
    if (c) {
        return 1;
    }
    exit(1);
}

/* ok: void functions are never asked to return a value */
void ok_void(int c) {
    if (c) {
        return;
    }
}

/* ok: a label after a return can be reached by a goto */
int ok_label_after_return(int c) {
    if (c) {
        goto tail;
    }
    return 1;
tail:
    return 2;
}

int main(void) {
    ok_void(0);
    return ok_switch_all_return(1) + ok_if_else(0) + ok_forever_unused(0);
}

/* ok: a one-armed if is fine when the function returns after it */
int ok_forever_unused(int c) {
    if (c) {
        return 1;
    }
    return 0;
}
