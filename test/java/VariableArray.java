/**
 * JavaLint tests: [code] in variable definition
 *
 * @author John Doe
 * @version 2015.1002
 * @since 1.7
 */
class Test {

    /**
     * Starts the application.
     *
     * @param args not used
     */
    public static void main(String [] args) {
        String a,b;
        String a, b;

        String a;
        String a; int b;

        String[] a;
        String[] a,b;
        String[] a,b,c;

        String [] a;
        String a[];
        String a[], b[];
        String a[], b;
        String a, b[];
        String a, b[],c;
    }
}
