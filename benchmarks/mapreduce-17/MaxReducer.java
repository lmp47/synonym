// Example based on MaxManual.java
// We want to check the Reducer for commutativity and associativity
public class Obj {
  int min;
  int valueLength;
  int getValue(int i);

  public void reduce(Obj o) {
    int i = 0;
    assume(o.min < 0);
    while (i < o.valueLength) {
      assume(o.min < o.getValue(i));
      i++;
    }
    int max = o.min;
    i = 0;
    while (i < o.valueLength) {
      // Need invariant:
      //   forall 0 <= j < i, max >= o.getValue(j)
      if (max < o.getValue(i)) {
        max = o.getValue(i);
      }
      i++;
    }
    return max;
  }

}
