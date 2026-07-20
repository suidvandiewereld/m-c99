/* A syntax error and a semantic error in one file.
 *
 * A parse failure used to stop the driver before sema ran, so the second
 * problem stayed invisible until the first was fixed and the build re-run.
 * Both must now come out of one run.
 *
 * 'unused_local' is here to pin the other half of the rule: sema warnings are
 * dropped when the parse failed, because a warning about code the parser could
 * not fully read is as likely to be an artefact as a finding.
 */

int f(void) { return 1 }

int g(void) {
    int unused_local = 7;
    return undeclared_thing;
}
