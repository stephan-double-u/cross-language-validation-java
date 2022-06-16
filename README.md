![Java CI](https://github.com/stephan-double-u/cross-language-validation-java/workflows/Java%20CI/badge.svg)

# Cross Language Validation Java 

This library implements the server side part of the [Cross Language Validation Schema](https://github.com/stephan-double-u/cross-language-validation-schema),
i.e. it contains:
- an rich API to define the different kinds of validation rules that are specified by that schema.
- a validator that checks the validation rules against the relevant objects.
- a serializer that produces a JSON representation of the defined validation rules according to that schema.

## Maven
### pom.xml
    <dependency>
      <groupId>de.swa</groupId>
      <artifactId>cross-language-validation</artifactId>
      <version>0.5.2</version>
    </dependency>

### settings.xml
Until this artifact is available on Maven Central, it can be downloaded by addinf this `server`to `settings.xml`.

Note: `<REMOVE>` must be remove from the public read access token: 

    <servers>
        <server>
            <id>github</id>
            <username>stephan-double-u</username>
            <password>ghp_<REMOVE>ZvGsEK3vqGUCXcyCQAVNuUupkKBju4242QoU</password>
        </server>
    </servers>

## Documentation
### Defining Validation Rules
Validation rules for a class of type _T_ are defined by creating a static object of type _ValidationRules_ where the
_Class_ object of type _T_ is the constructor argument. The different kinds of rules are then defined in a _static 
initializer block_.

For an example class _Foo_ this could be done within the class itself:

```java
public class Foo {
    public static final ValidationRules<Foo> RULES = new ValidationRules<>(Foo.class);
    static {
        RULES.mandatory("fooProperty");
    }
    private String someProperty;
    // getter, setter etc. omitted
}
```
If you prefer working with POJOs, this could be done in another class as well, e.g. in a class where you collect 
all rules for all corresponding classes:
```java
public class Rules {
    public static final ValidationRules<Foo> FOO = new ValidationRules<>(Foo.class);
    static {
        RULES.mandatory("fooProperty");
    }
    public static final ValidationRules<Bar> BAR = new ValidationRules<>(Bar.class);
    static {
        RULES.immutable("barProperty");
    }
}
```
#### Mandatory Rules
TODO
```java
   RULES.mandatory("name");
```
#### Immutable Rules
TODO
```java
   RULES.immutable("type");
```

#### Content Rules
TODO
```java
   RULES.content("name", Size.minMax(1,100));
```

#### Update Rules
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

### Serialize Rules
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
## TODOs
- Bring to Maven Central 
- Document
- Write more tests
- Release
- ...
