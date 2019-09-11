/**
 * JavaLint
 *
 * @author Jussi Pohjolainen
 * @version 2015.1002
 * @since 1.7
 */
class Main {

    /**
     * Starts the app.
     *
     * @param args not used
     */
    public static void main(String [] args) {
        // Javalint works
        test("1, 2, 3");

        // Javalint fails:
        // JavaLintBug.java:0020: [code] in method call, no space after comma
        //     test("1,2,3");
        test("1,2,3");
        test(a,"1,2,3", 2, a);
        test(1,"2",a);
    }

    /**
     * Tests javalint.
     *
     * @param a nothing
     */
    public static void test(String a) {
        System.out.println(a);
    }
}
