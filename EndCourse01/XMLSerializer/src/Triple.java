public class Triple<T1, T2, T3> extends Pair<T1, T2> {
    public T3 third;

    public Triple(T1 first, T2 second) {
        super(first, second);
        third = null;
    }

    public Triple(T1 first, T2 second, T3 third){
        super(first, second);
        this.third = third;
    }
}
