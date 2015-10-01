/**
 * JavaLint tests: [code] in method call ....
 *
 * @author John Doe
 * @version 2015.1002
 * @since 1.7
 */
class Methodcall1 {

    /**
     * Starts the application.
     *
     * @param args not used
     */
    public static void main(String[] args) {
        test(1,2);
        test( 1,2 );
        test(1, 2);
        test(1, 2 );
        test( 1, 2 );
        test("1, 2, 3");
        test("1,2,3");
        test(1,"1,2,3");
        test(1, "1,2,3");
        test("1,2,3",1);
        test("1,2,3", 1);
        test(1,"2",3);
        test(1, "2",3);
        test(1, "2", 3);
        test( 1, "2", 3);
        test( 1, "2", 3 );
    }
}
