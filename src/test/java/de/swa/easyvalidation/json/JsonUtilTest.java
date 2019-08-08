package de.swa.easyvalidation.json;

import de.swa.easyvalidation.ValidationConditions;
import org.junit.Test;

import static de.swa.easyvalidation.ValidationConditions.serializeToJson;
import static org.junit.Assert.*;

public class JsonUtilTest {

    ValidationConditions<Foo> fooValidationConditions = new ValidationConditions<>(Foo.class);
    ValidationConditions<Bar> barValidationConditions = new ValidationConditions<>(Bar.class);

    @Test
    public void toJson_ShouldCreateOneElementArray() {
        final String expected = "{" + fooValidationConditions.serializeToJson() + "}";
        assertEquals(expected, serializeToJson(fooValidationConditions));

    }
    @Test
    public void toJson_ShouldCreateCommaSeparatedArray() {
        final String expected = "{" + fooValidationConditions.serializeToJson() + "," + barValidationConditions.serializeToJson() + "}";
        assertEquals(expected, serializeToJson(fooValidationConditions, barValidationConditions));
    }

    class Foo {}
    class Bar {}
}