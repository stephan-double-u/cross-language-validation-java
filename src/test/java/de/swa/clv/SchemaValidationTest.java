package de.swa.clv;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.networknt.schema.JsonSchema;
import com.networknt.schema.JsonSchemaFactory;
import com.networknt.schema.SpecVersion;
import com.networknt.schema.ValidationMessage;

import java.io.InputStream;
import java.util.Set;

// Kudos to https://www.javatpoint.com/json-validator-java
public class SchemaValidationTest {

    private static InputStream inputStreamFromClasspath(String path) {
        return Thread.currentThread().getContextClassLoader().getResourceAsStream(path);
    }

    // TODO migrate to unit test
    public static void main(String[] args) throws Exception {

        ObjectMapper objectMapper = new ObjectMapper();

        JsonSchemaFactory schemaFactory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V201909);

        try (
                InputStream jsonStream = inputStreamFromClasspath("schema-unzipped/cross-language-validation-schema-0.2/cross-language-validation-example.json");
                InputStream schemaStream = inputStreamFromClasspath("schema-unzipped/cross-language-validation-schema-0.2/cross-language-validation-schema.json")
        ) {
            JsonNode json = objectMapper.readTree(jsonStream);
            JsonSchema schema = schemaFactory.getSchema(schemaStream);

            // Validation may create validation message
            Set<ValidationMessage> validationResult = schema.validate(json);

            // show the validation errors
            if (validationResult.isEmpty()) {
                System.out.println("There is no validation errors");
            } else {
                validationResult.forEach(vm -> System.out.println(vm.getMessage()));
            }
        }
    }
}
