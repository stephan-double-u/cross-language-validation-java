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
      <version>0.2.1</version>
    </dependency>

### settings.xml
    <servers>
        <server>
            <id>github</id>
            <username>stephan-double-u</username>
            <password>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</password>
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
        RULES.mandatory("someProperty");
    }
    private String someProperty;
    // getter, setter etc. omitted
}
```
But if you prefer working with POJOs, this could be done in another class as well, e.g. in a class where you collect 
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
TODO

### Validating Rules
TODO

#### Serialize Rules
TODO
```javascript
    ValidationRules.serializeToJson(Foo.RULES /*, Bar.RULES, ... */);
```

## TODOs
- Bring to Maven Central 
- Document
- Write more tests
- Release
- ...
