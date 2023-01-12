import java.lang.reflect.Field;
import java.util.*;

public class XMLSerializer {
    public static void serialize(Object[] arr, String filename){
        for (Object o : arr) {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
            XMLTreeNode.makeNode(o).appendToBuilder(stringBuilder, 0);
            System.out.println(stringBuilder);
        }
    }

    public static void indent(StringBuilder bldr, int indentation){
        for(int i = 0; i < indentation; i++){
            bldr.append("\t");
        }
    }

    private static class ClassInfo{
        private static final Map<Class, ClassInfo> cache = new HashMap<>();
        private static final ClassInfo notXMLable = new ClassInfo(new Object()){};
        final String className;
        final List<Triple<String, String, String>> fields;

        private ClassInfo(Object obj){
            Class<?> cls = obj.getClass();
            this.fields = new ArrayList<>();
            if(cls.getAnnotation(XMLable.class) != null) {
                this.className = cls.getSimpleName();
                Arrays.stream(cls.getDeclaredFields()).forEach(field -> {
                    XMLfield annotation = field.getAnnotation(XMLfield.class);
                    if(annotation == null){
                        return;
                    }
                    this.fields.add(new Triple<>(field.getName(), annotation.name().equals("") ? field.getName() : annotation.name(), annotation.type()));
                });
            }else{
                this.className = "notXMLable";
            }
        }

        private static ClassInfo getClassInfo(Object obj){
            Class<?> cls = obj.getClass();
            if(cls.getAnnotation(XMLable.class) != null) {
                ClassInfo info = ClassInfo.cache.get(cls);
                if(info == null){
                    info = new ClassInfo(obj);
                    ClassInfo.cache.put(cls, info);
                }
                return info;
            }
            return ClassInfo.notXMLable;
        }
    }

    private static abstract class XMLTreeNode{
        protected String elementName;
        protected Map<String, String> attributes;

        public static XMLTreeNode makeNode(Object obj){
            return makeNode(ClassInfo.getClassInfo(obj), obj);
        }

        private static XMLTreeNode makeNode(ClassInfo info, Object obj){
            XMLTreeNodeChildren out = new XMLTreeNodeChildren();

            out.elementName = info.className;
            out.attributes = new HashMap<>(info.fields.size());
            info.fields.forEach(p -> {
                try {
                    Field f = obj.getClass().getDeclaredField(p.first);
                    f.setAccessible(true);
                    XMLTreeNodeValue child = new XMLTreeNodeValue(
                            p.second,
                            p.third,
                            f.get(obj).toString()
                    );
                    out.children.add(child);
                } catch (NoSuchFieldException | IllegalAccessException ignored) {} //Failed to serialize, ignore
            });

            return out;
        }

        public void appendToBuilder(StringBuilder bldr, int indentation) {
            XMLSerializer.indent(bldr, indentation);
            bldr.append("<").append(this.elementName);
            attributes.forEach((name, value) -> bldr.append(" ").append(name).append("=\"").append(value).append("\""));
            if(isEmpty()) {
                bldr.append("/>\n");
            }else{
                bldr.append(">\n");
                this.appendInnerToBuilder(bldr, indentation + 1);
                XMLSerializer.indent(bldr, indentation);
                bldr.append("</").append(this.elementName).append(">\n");
            }
        }

        protected abstract void appendInnerToBuilder(StringBuilder bldr, int indentation);

        protected abstract boolean isEmpty();
    }

    private static class XMLTreeNodeValue extends XMLTreeNode{
        private final String value;

        XMLTreeNodeValue(String name, String type, String value) {
            this.elementName = name;
            this.attributes = new HashMap<>();
            this.attributes.put("type", type);
            this.value = value;
        }

        @Override
        protected void appendInnerToBuilder(StringBuilder bldr, int indentation) {
            XMLSerializer.indent(bldr, indentation);
            bldr.append(value);
            bldr.append("\n");
        }

        @Override
        protected boolean isEmpty() {
            return value == null;
        }
    }

    private static class XMLTreeNodeChildren extends XMLTreeNode{
        private final List<XMLTreeNode> children = new ArrayList<>();

        XMLTreeNodeChildren() {

        }

        @Override
        protected void appendInnerToBuilder(StringBuilder bldr, int indentation) {
            children.forEach(c -> c.appendToBuilder(bldr, indentation));
        }

        @Override
        protected boolean isEmpty() {
            return this.children.size() == 0;
        }
    }
}
