/*
 * Based on http://stackoverflow.com/questions/16809000/how-to-make-the-compareto-method-respect-the-general-contract
 * 
 */

public class Chromosome implements Comparator<Chromosome>{
  int getScore(int num);
  int isNull;
  int min12;
  int min13;
  int min23;
  int min123;
  int id;
  int comp2;
  int comp3;

  public int compare(Chromosome o1, Chromosome o2, Chromosome o3) {
      assume (o1.id != o2.id);
      assume (o1.id != o3.id);
      assume (o2.id != o3.id);
      if (o1.id < o2.id)
          assume(o1.min12 == o1.id);
      else
          assume(o1.min12 == o2.id);
      if (o1.id < o3.id)
          assume(o1.min13 == o1.id);
      else
          assume(o1.min13 == o3.id);
      if (o2.id < o3.id)
          assume(o1.min23 == o2.id);
      else
          assume(o1.min23 == o3.id);
      if (o1.id < o1.min23)
          assume(o1.min123 == o1.id);
      else
          assume(o1.min123 == o1.min23);
      if(o1.isNull == 0)
          return(1);
      if(o2.isNull == 0)
          return(1);
      if(o3.isNull == 0)
          return(1);
      int comp = 0;
      int i = 0;
      while(i < 5){
          if (Double.compare(o1.getScore(i), o2.getScore(i)) > 0 &&
              (Double.compare(o1.getScore(i), o3.getScore(i)) > 0))
            return o1.id;
          if (Double.compare(o2.getScore(i), o1.getScore(i)) > 0 &&
              (Double.compare(o2.getScore(i), o3.getScore(i)) > 0))
            return o2.id;
          if (Double.compare(o3.getScore(i), o1.getScore(i)) > 0 &&
              (Double.compare(o3.getScore(i), o2.getScore(i)) > 0))
            return o3.id;
          i++;
      }

      return o1.min123;

      return 0;
  }
}
