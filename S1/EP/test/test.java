class test {
	public static void main(String[] args) {
    short s = 12;
    short t = 14;
    s = (short)(t + s);
  }

  public static void myst(int n){
    System.out.println(n);
    if(n>1){myst(n-1);myst(n-2);}
  }
}
