class PersonTester {
  public static void main(String[] args) {
    Person a = new Person("George");
    Person b = new Person("John");
    Person c = new Person("George");


    System.out.println(a.equals(b));
    System.out.println(a.equals(c));

    System.out.println(a == b);
    System.out.println(a == c);

    
  }

  class Person {
    private String name;

    public Person(String name) {
      this.name = name;
    }

    public boolean equals(Person another) {
      return this.name.equals(another.getName());
    }
  }
}
