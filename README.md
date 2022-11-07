![Java CI](https://github.com/stephan-double-u/cross-language-validation-java/workflows/Java%20CI/badge.svg)

# Cross Language Validation Java 

This library implements the producer part and the validator part of the &#10169;
[Cross Language Validation Schema](https://github.com/stephan-double-u/cross-language-validation-schema), 
i.e. it provides:
- a rich API to define all kinds of validation rules that are specified by that schema
- a validator that checks the validation rules against the relevant objects
- a serializer that produces the JSON representation of the validation rules according to that schema.

The requirements for an implementation of that schema are described in the schema documentation.

## Maven
### pom.xml
    <dependency>
      <groupId>de.swa</groupId>
      <artifactId>cross-language-validation</artifactId>
      <version>0.9.2</version>
    </dependency>

### settings.xml
Until this artifact is available on Maven Central, it can be downloaded by addinf this `server`to `settings.xml`.

Note: `<REMOVE>` must be removed from the public read access token: 

    <servers>
        <server>
            <id>github</id>
            <username>stephan-double-u</username>
            <password>ghp_<REMOVE>OakGskz7Fv8r328l92rjTJAWKKiQOa2p80sA</password>
        </server>
    </servers>

## Documentation
The examples used in this documentation are based on the example objects described in &#10169;
[schema dokumentation](https://github.com/stephan-double-u/cross-language-validation-schema#example-objects).

### Defining validation rules
Validation rules for a class or record of type _T_ are defined by creating a static object of type _ValidationRules_ 
where the _Class_ object of type _T_ is the constructor argument. The different kinds of rules are then defined in 
a _static initializer block_ with on of the numerous variants of the methods `mandatory(⋯)`, `immutable(⋯)`, 
`content(⋯)` resp. `update(⋯)`.

> For an example class _Article_ this could be done within the class itself:
>
> ```java
> public class Article {
>     public static final ValidationRules<Foo> RULES = new ValidationRules<>(Article.class);
>     static {
>         RULES.mandatory("name");
>     }
>     private String name;
>     // getter, setter etc. omitted
> }
> ```
If you prefer working with POJOs, this could be done in a separate class as well, e.g. in a class where you 
collect all rules for all classes resp. records which have validation rules:
```java
public class Rules {
    public static final ValidationRules<Article> ARTICLE_RULES = new ValidationRules<>(Article.class);
    static {
        ARTICLE_RULES.mandatory("name");
    }
    public static final ValidationRules<MedicalSet> MEDICAL_SET_RULES = new ValidationRules<>(MedicalSet.class);
    static {
        MEDICAL_SET_RULES.immutable("id");
    }
}
```
In all the following examples the `static` parts are omitted for the sake of compactness.

#### Mandatory rules
Validation rule for mandatory properties are defined with one of the `mandatory(⋯)` methods. The methods have 1 to 3
arguments
- the 1st required argument is the name of the property that should be mandatory, e.g.
  > `ARTICLE_RULES.mandatory("name");`
- the optional next argument is of type `Permissions`,Permissions.any(java.lang.Enum<?>...) e.g.
  > `ARTICLE_RULES.mandatory("name", Permissions.any("EXPERT"));`
  > `ARTICLE_RULES.mandatory("name", Permissions.all("EXPERT"));`
  > `ARTICLE_RULES.mandatory("name", Permissions.none("EXPERT"));`
- the optional next argument is of type `Permissions`
```java
   ARTICLE_RULES.mandatory("name");
```
#### Immutable rules
TODO
```java
   RULES.immutable("type");
```

#### Content rules
TODO
```java
   RULES.content("name", Size.minMax(1,100));
```

#### Update rules
TODO
```java
   RULES.update("status", Equals.any(StatusEnum.START, StatusEnum.STOP),
        Condition.of("status",  Equals.any(StatusEnum.START)));
```

#### Permissions based rules 
TODO

A validation rule may depend on certain user privileges, i.e. a rule should only be checked if the user (who made the
insertion or update request) has a certain role, permission or authority (or whatever you want to call it).

For mandatory and immutable rules permissions can be defined as second argument, either as enum values or as strings:
```java
   RULES.mandatory("email", Permissions.any(Role.NEWBIE));
   RULES.immutable("name", Permissions.any("REVIEWER", "ReadOnly") );
```
For content and update rules permissions can be defined as third argument, either as enum values or as strings:
```java
   RULES.content("name", Size.minMax(1,10),
        Permissions.any("NEWBIE"));
   RULES.update("status", Equals.any(StatusEnum.values()),
        Permissions.any("ADMIN") );
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
   RULES.mandatory("name").doNotSerialize();
```

## TODOs
- Bring to Maven Central 
- Documentation
- Write more tests
- Release Notes

