import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public abstract class JobScheduler<K,V> {
    public static void main(String[] args) throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException, InstantiationException, IllegalAccessException {
        JobScheduler js = (JobScheduler) Class.forName(System.getProperty("sun.java.command")).getDeclaredConstructor().newInstance();
        js.output(js.collect(js.compute(js.emit())));
    }

    private Stream<Pair<K,V>> compute(Stream<AJob<K,V>> stream){
        return stream.flatMap(AJob::execute);
    }
    private Stream<Pair<K, List<V>>> collect(Stream<Pair<K,V>> stream){
        return stream.collect(
            Collectors.toMap(
                Pair::getKey,
                p -> {
                    ArrayList<V> al = new ArrayList<>();
                    al.add(p.getValue());
                    return al;
                },
                (value1, value2) -> {
                    value1.addAll(value2);
                    return value1;
                }
            )
        ).entrySet().stream().map(e -> new Pair<>(e.getKey(), e.getValue()));
    }

    abstract Stream<AJob<K,V>> emit();
    abstract void output(Stream<Pair<K, List<V>>> stream);
}
