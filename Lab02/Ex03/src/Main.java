import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class Main {
	public static void main(String[] args){
		var in = new Scanner(System.in);
		String beanName = in.nextLine();
		Class bean = null;
		try {
			bean = Class.forName(beanName);
		}catch(ClassNotFoundException cnfe){
			System.err.println("Invalid class name");
			System.exit(1);
		}
		

	}
	
	public static void getProperties(Class bean){
		Map<String, Integer> props = new HashMap<>();
		for(Method m : bean.getDeclaredMethods()){
			String methodName = m.getName();
			if(methodName.startsWith("get")){
				String propName = methodName.substring(3);
				props.merge(propName, 1, (a, b) -> a | b);
			}else if(methodName.startsWith("set")){
				String propName = methodName.substring(3);
				props.merge(propName, 2, (a, b) -> a | b);
			}
		}
		
	}
}