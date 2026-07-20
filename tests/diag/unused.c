/* 'scratch' is never read. 'used' is. '_intentional' opts out by name.
 *
 * 'written' is assigned and never read, and must NOT warn: any resolution of
 * the name counts as a use, writes included, which is what gcc's
 * -Wunused-variable does too. Distinguishing set-but-never-read is a separate,
 * stricter check (gcc's -Wunused-but-set-variable) and would need its own
 * group; this one warns only when the name is never touched again at all.
 */
int side_effect(void);

int main(void) {
    int used = 1;
    int scratch = 2;
    int _intentional = 3;
    int written;
    written = side_effect();
    return used;
}

int side_effect(void) { return 4; }
