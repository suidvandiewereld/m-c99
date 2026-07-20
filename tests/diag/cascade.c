/* Two unrelated missing semicolons, on line 15 and line 21.
 *
 * The parser used to report an error for every token of the wreckage after a
 * failed expect(), so the first mistake alone produced three. It must now
 * report exactly two: one per real problem.
 *
 * Both halves matter. Asserting only "one error" would pass just as well if
 * the parser gave up at the first mistake and never looked at the rest of the
 * file, which is the opposite of recovering. Requiring the second, independent
 * error proves it carried on; requiring exactly two proves it did not cascade.
 *
 * Both errors are deliberately syntax errors. A parse failure stops the driver
 * before sema runs, so a semantic error here would never be reached and the
 * test would be asserting the wrong thing.
 */

int f(void) { return 1 }

int g(void) { return 2; }

int h(void) { return 3 }

int i(void) { return 4; }
