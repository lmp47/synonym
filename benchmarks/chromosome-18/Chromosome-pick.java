/*
 * Based on http://stackoverflow.com/questions/16809000/how-to-make-the-compareto-method-respect-the-general-contract
 * 
 */

public class Chromosome implements Comparator<Chromosome>{
  int getScore(int num);
  int isNull;
  int id;

  public int compare(Chromosome o1, Chromosome o2) {
      assume(o1.id != o2.id);
      if(o1.isNull == 0)
          return(1);
      if(o2.isNull == 0)
          return(1);
      int comp12 = 0;
      int i = 0;
      while(i < 5){
          comp12 = Double.compare(o1.getScore(i), o2.getScore(i));
          if (comp12>0)
            return o1.id;            
          if (comp12<0)
            return o2.id;            
          i++;
      }

      if (o1.id < o2.id)
        return o1.id;
      return o2.id;
  }
}
