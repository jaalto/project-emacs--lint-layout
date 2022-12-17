/**
 * JavaLint tests: [code] in method call ....
 *
 * @author John Doe
 * @version 2015.1002
 * @since 1.7
 */
class Variables {

    /**
     * Starts the application.
     *
     * @param args not used
     */
    public static void main(String[] args) {

    String avgTemp = ""; // Comment
    String count = ""; // Comment
    String 400cc = ""; // Error: not a statement, ';' expected
    int 400cc = 10; // Error: not a statement, ';' expected
    String $test$ = ""; // Comment
    String private = ""; // Error: not a statement, ';' expected
    String &that_One = ""; // Comment
    String perhaps#? = ""; // Error: illegal character: \35
    String try_Once/more = ""; // Error: ';' expected
    String isThisOkVariable_name = ""; // Comment
    String int = ""; // Error: not a statement, ';' expected
    }
}
