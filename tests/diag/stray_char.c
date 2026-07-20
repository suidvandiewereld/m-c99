/* A stray character used to return TkEof and silently discard the rest of the
 * translation unit. The later error proves lexing continued. */
int main(void) {
    int a @ = 1;
    return undeclared_after_stray;
}
