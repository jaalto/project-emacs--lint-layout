/**
 * JavaLint tests: [code] in variable definition
 *
 * @author John Doe
 * @version 2015.1002
 * @since 1.7
 */
class MethodArray {

    /**
     * Describes something.
     *
     * @param args not used
     */
    public static void test1(String args[]) {
        // Something
    }

    /**
     * Describes something.
     *
     * @param args not used
     */
    public static void test2(String [] args) {
        // Something
    }

    /**
     * Describes something.
     *
     * @param astr not used
     * @param args not used
     */
    public static void test2(String str, String args[]) {
        // Something
    }

    /**
     * Describes something.
     *
     * @param one not used
     * @param two not used
     * @param three not used
     */
    public static void test1(int one, int[] two, int three) {
        // Something
    }

    /**
     * Describes something.
     *
     * @param one not used
     * @param two not used
     * @param three not used
     */
    public static void test1(int one, int two[], int three) {
        // Something
    }

    /**
     * Describes something.
     *
     * @param args not used
     */
    public static void main(String[] args) {
        // Something
    }
}
