/*
 * Based on http://stackoverflow.com/questions/23907134/comparing-two-arrays-using-dictionary-order-in-an-array-of-arrays-java
 * 
 */

public class Prob{
  private String b1;
  private String b2;
  private String b3;
  private String b4;
  private String b5;
  private String b6;
  private String b7;
  private String b8;
  private String b9;

  boolean is_set_b1();
  boolean is_set_b2();
  boolean is_set_b3();
  boolean is_set_b4();
  boolean is_set_b5();
  boolean is_set_b6();
  boolean is_set_b7();
  boolean is_set_b8();
  boolean is_set_b9();

  public boolean equals(Prob o1, Prob o2) {
    boolean this_b1 = true && o1.is_set_b1();
    boolean that_b1 = true && o2.is_set_b1();
    if (this_b1 || that_b1) {
      if (!(this_b1 && that_b1))
        return false;
      if (equals(o1.b1,o2.b1) == 0)
        return false;
    }

    boolean this_b2 = true && o1.is_set_b2();
    boolean that_b2 = true && o2.is_set_b2();
    if (this_b2 || that_b2) {
      if (!(this_b2 && that_b2))
        return false;
      if (equals(o1.b2,o2.b2) == 0)
        return false;
    }

    boolean this_b3 = true && o1.is_set_b3();
    boolean that_b3 = true && o2.is_set_b3();
    if (this_b3 || that_b3) {
      if (!(this_b3 && that_b3))
        return false;
      if (equals(o1.b3,o2.b3) == 0)
        return false;
    }

    boolean this_b4 = true && o1.is_set_b4();
    boolean that_b4 = true && o2.is_set_b4();
    if (this_b4 || that_b4) {
      if (!(this_b4 && that_b4))
        return false;
      if (equals(o1.b4,o2.b4) == 0)
        return false;
    }

    boolean this_b5 = true && o1.is_set_b5();
    boolean that_b5 = true && o2.is_set_b5();
    if (this_b5 || that_b5) {
      if (!(this_b5 && that_b5))
        return false;
      if (equals(o1.b5,o2.b5) == 0)
        return false;
    }

    boolean this_b6 = true && o1.is_set_b6();
    boolean that_b6 = true && o2.is_set_b6();
    if (this_b6 || that_b6) {
      if (!(this_b6 && that_b6))
        return false;
      if (equals(o1.b6,o2.b6) == 0)
        return false;
    }

/*
    boolean this_b7 = true && o1.is_set_b7();
    boolean that_b7 = true && o2.is_set_b7();
    if (this_b7 || that_b7) {
      if (!(this_b7 && that_b7))
        return false;
      if (equals(o1.b7,o2.b7) == 0)
        return false;
    }

    boolean this_b8 = true && o1.is_set_b8();
    boolean that_b8 = true && o2.is_set_b8();
    if (this_b8 || that_b8) {
      if (!(this_b8 && that_b8))
        return false;
      if (equals(o1.b8,o2.b8) == 0)
        return false;
    }

    boolean this_b9 = true && o1.is_set_b9();
    boolean that_b9 = true && o2.is_set_b9();
    if (this_b9 || that_b9) {
      if (!(this_b9 && that_b9))
        return false;
      if (equals(o1.b9,o2.b9) == 0)
        return false;
    }

    return true;
*/
  }

}

