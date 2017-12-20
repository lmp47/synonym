// Example based on CountMaxHadoop.java
// We want to check the Reducer for commutativity and associativity
public class Obj {
  int min;
  int valueLength;
  int getValueFirst(int i);
  int getValueSecond(int i);

  public void reduce(Obj o) {
    int i = 0;
    assume(o.min < 0);
    while (i < o.valueLength) {
      assume(o.min < o.getValueFirst(i));
      assume(o.min < o.getValueSecond(i));
      i++;
    }
    int first = o.min;
    int second = 0;
    i = 0;
    while (i < o.valueLength) {
      if (first < o.getValueFirst(i)) {
        first = o.getValueFirst(i);
        second = o.getValueSecond(i);
      }
      else if (first == o.getValueFirst(i)) {
        second += o.getValueSecond(i);
      }
      i++;
    }
    return second;
  }

}
