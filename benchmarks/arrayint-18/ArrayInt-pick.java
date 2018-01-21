/*
 * Based on http://stackoverflow.com/questions/23907134/comparing-two-arrays-using-dictionary-order-in-an-array-of-arrays-java
 * 
 */

public class AInt implements Comparator<AInt>{
   int length;
   int get(int pos);
   int id;
   int min12;
   
   public int compare(AInt o1, AInt o2){
     assume (o1.id != o2.id);
     if (o1.id <= o2.id) {
         assume (o1.min12 == o1.id);
     } else {
         assume (o1.min12 == o2.id);
     }
     int index, aentry, bentry;
     index = 0;
     
     while ((index < o1.length) && (index < o2.length)) {
       aentry = o1.get(index);
       bentry = o2.get(index);
       if (aentry < bentry) {
           return o2.id;
       }
       if (aentry > bentry) {
           return o1.id;
       }
       index++;
     }
     
     if (o1.length < o2.length) {
       return o2.id;
     }
     if (o1.length > o2.length) {
       return o1.id;
     }
     return o1.min12;
   }
}

