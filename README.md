![Java CI](https://github.com/stephan-double-u/cross-language-validation-java/workflows/Java%20CI/badge.svg)

# Cross Language Validation Java 

This library implements the producer and the validator part of the &#10169;
[Cross Language Validation Schema](https://github.com/stephan-double-u/cross-language-validation-schema), 
i.e. it provides:
- a rich API to define all kinds of validation rules that are specified by that schema.
- a validator that checks the validation rules against the relevant objects.
- a serializer that produces the JSON representation of the validation rules according to that schema.

The requirements for an implementation of that schema are described in the schema documentation.

The implementation supports defining validation rules for `Records` and therefore requires at least _Java 17_. 

## Maven
Until this artifact is available on Maven Central, it can be used with by building it with 
[JitPack](https://jitpack.io/).

Add the `repository jitpack.io` and the `dependency cross-language-validation-java` to the `pom.xml` file.

    <repositories>
      <repository>
        <id>central</id>
        <url>https://repo.maven.apache.org/maven2</url>
      </repository>
      <repository>
        <id>jitpack.io</id>
        <url>https://jitpack.io</url>
      </repository>
    </repositories>

    <dependencies>
      <dependency>
        <groupId>com.github.stephan-double-u</groupId>
        <artifactId>cross-language-validation-java</artifactId>
        <version>v0.10.2</version>
      </dependency>
    </dependencies>

## Documentation
The examples used in this documentation are based on the example objects described in &#10169;
[schema documentation](https://github.com/stephan-double-u/cross-language-validation-schema#example-objects).

### Defining validation rules
Validation rules for a class or record of type _T_ are defined by creating a static object of type _ValidationRules_ 
where the _Class_ object of type _T_ is the constructor argument. The different kinds of rules are then defined in 
a _static initializer block_ with one of the numerous variants of the methods `mandatory(⋯)`, `immutable(⋯)`, 
`content(⋯)` resp. `update(⋯)`.

> For an example class _Article_ this could be done within the class itself:
>
> ```java
> public class Article {
>     public static final ValidationRules<Foo> rules = new ValidationRules<>(Article.class);
>     static {
>         rules.mandatory("name");
>     }
>     private String name;
>     // getter, setter etc. omitted
> }
> ```
If you prefer working with POJOs, this could be done in a separate class as well, e.g. in a class where you 
collect all rules for all classes resp. records which have validation rules:
```java
public class Rules {
    public static final ValidationRules<Article> article_rules = new ValidationRules<>(Article.class);
    static {
        article_rules.mandatory("name");
    }
    public static final ValidationRules<MedicalSet> medical_set_rules = new ValidationRules<>(MedicalSet.class);
    static {
        medical_set_rules.immutable("id");
    }
}
```
In all the following examples the `static` block syntax is omitted for the sake of compactness.

#### Mandatory rules
Validation rule for mandatory properties are defined with one of the `mandatory(⋯)` methods. The methods have 1 to 3
arguments.
- The 1st required argument is the name of the property that should be mandatory, e.g.
  ```javascript
  rules.mandatory("name");
  ```
  This imposes the **implicit constraint** _that the property must not be _null_ when the rule is evaluated_.<br><br>

- The 2nd argument of type `Permissions` is optional.<br>
  A validation rule may depend on certain user permissions, i.e. that a rule should be checked only if the user 
  (who made the insertion or update request) has a certain role, permission, or authority (or whatever you want to   
  call it).<br>
  This argument can be used to specify such conditions, namely that a user has either any, 
  or all, or none of the permissions from a given list. <br>
  The `Permissions` class provides the methods `any(⋯)`, `all(⋯)` and `none(⋯)` for this.<br>
  Each method has one _varargs_ parameter either of type `String` or `Enum<?>`, e.g.
  ```javascript
  rules.mandatory("name", Permissions.any("EXPERT", "MANAGER"));
  rules.mandatory("number", Permissions.all("EXPERT", "MANAGER"));
  rules.mandatory("staus", Permissions.none(Permission.EXPERT, Permission.MANAGER));
  ```
- The next (2nd resp. 3rd) argument is of type `Conditions`, `ConditionsGroup` or `ConditionsTopGroup`, depending on
  the number of conditions that needs to be specified for the rule and how these rules are logically connected.<br>
  _Condition objects_ specify conditions for properties of the object for which the rule is defined that must be 
  satisfied before the implicit property constraint is even checked.<br>
  A condition object consists of a _property name_ and one of the numerous _constraint objects_ (like `Size.min(1)`)<br>
  a single condition object can be defined like this:
  ```javascript
  rules.mandatory("name", Permissions.any("EXPERT", "MANAGER"), 
          Condition.of("status", Equals.none(NEW)));
  ```
  Several condition objects that are all logically connected with either _AND_ or _OR_, can be defined
  with the help of a `ConditionsGroup` object, e.g.:
  ```javascript
  rules.mandatory("name",
          ConditionsGroup.OR(
                  Condition.of("status", Equals.none(NEW)),
                  Condition.of("number", Equals.notNull())));
  ```
  If the condition objects are logically connected with _AND_ **and** _OR_, a `ConditionsTopGroup` object can be used,
  e.g.:
  ```javascript
  rules.mandatory("name",
          ConditionsTopGroup.OR(
                  ConditionsGroup.AND(
                          Condition.of("status", Equals.none(NEW)),
                          Condition.of("number", Equals.notNull())),
                  ConditionsGroup.AND(
                          Condition.of("medicalSet", Equals.notNull()))));
  ```

#### Immutable rules
TODO
```java
   rules.immutable("type");
```

#### Content rules
TODO
```java
   rules.content("name", Size.minMax(1,100));
```

#### Update rules
TODO
```java
   rules.update("status", Equals.any(StatusEnum.START, StatusEnum.STOP),
        Condition.of("status",  Equals.any(StatusEnum.START)));
```

### Serialize rules
Validation rules can be serialized to JSON according to the Cross Language Validation Schema.

Either by calling _serializeToJson()_ on the rules object itself:
```java
    ValidationRules<ClassOne> rules = new ValidationRules<>(ClassOne.class);
    final String rulesAsJson = rules.serializeToJson();
```
or, for multiple rules objects, by calling the static _varargs_ method ValidationRules#serializeToJson
```java
    ValidationRules<ClassOne> rulesClassOne = new ValidationRules<>(ClassOne.class);
    ValidationRules<ClassTwo> rulesClassTwo = new ValidationRules<>(ClassTwo.class);
    ValidationRules.serializeToJson(rulesClassOne, rulesClassTwo);
```
### Disable serialization for single rules
TODO

Sometimes it is either not possible, nor not necessary, nor not desired to serialize a rule.

The CLV Demp App contains an example with explanation for the first two cases.

If a rule should not be serialized, the method _doNotSerialize()_ can be called for any rule.
```java
   rules.mandatory("name").doNotSerialize();
```

## TODOs
- Bring to Maven Central 
- Documentation
- Release Notes

