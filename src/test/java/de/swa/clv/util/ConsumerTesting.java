package de.swa.clv.util;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ConsumerTesting {

    private static final String testJson = "  {'foo':'x','bar':123,'zoo':{'one':'y'},'baz':[1,2,3]}".replace('\'', '"');
    private static final String testJson2 = "{\n" +
            "  \"schema-version\": \"0.2\",\n" +
            "  \"mandatoryRules\": {\n" +
            "    \"reservation\": {\n" +
            "      \"customer.name[0]\": [\n" +
            "        {\n" +
            "          \"condition\": {\n" +
            "            \"property\": \"aBoolean\",\n" +
            "            \"constraint\": {\n" +
            "              \"type\": \"EQUALS_ANY\",\n" +
            "              \"values\": [\n" +
            "                12345678901234567890,\n" +
            "                123456789.123,\n" +
            "                -12345.6789e-5\n" +
            "              ]\n" +
            "            }\n" +
            "          }\n" +
            "        },\n" +
            "        {\n" +
            "          \"permissions\": {\n" +
            "            \"type\": \"ANY\",\n" +
            "            \"values\": [\n" +
            "              \"aaa\"\n" +
            "            ]\n" +
            "          }\n" +
            "        }\n" +
            "      ],\n" +
            "      \"id\": [\n" +
            "        {\n" +
            "          \"permissions\": {\n" +
            "            \"type\": \"ANY\",\n" +
            "            \"values\": [\n" +
            "              \"aaa\"\n" +
            "            ]\n" +
            "          }\n" +
            "        },\n" +
            "        {\n" +
            "          \"permissions\": {\n" +
            "            \"type\": \"ANY\",\n" +
            "            \"values\": [\n" +
            "              \"bbb\"\n" +
            "            ]\n" +
            "          },\n" +
            "          \"conditionsGroup\": {\n" +
            "            \"operator\": \"AND\",\n" +
            "            \"conditions\": [\n" +
            "              {\n" +
            "                \"property\": \"someString\",\n" +
            "                \"constraint\": {\n" +
            "                  \"type\": \"SIZE\",\n" +
            "                  \"min\": 1,\n" +
            "                  \"max\": 3\n" +
            "                }\n" +
            "              },\n" +
            "              {\n" +
            "                \"property\": \"articleArray\",\n" +
            "                \"constraint\": {\n" +
            "                  \"type\": \"SIZE\",\n" +
            "                  \"min\": 2\n" +
            "                }\n" +
            "              },\n" +
            "              {\n" +
            "                \"property\": \"someMap\",\n" +
            "                \"constraint\": {\n" +
            "                  \"type\": \"SIZE\",\n" +
            "                  \"max\": 2\n" +
            "                }\n" +
            "              }\n" +
            "            ]\n" +
            "          }\n" +
            "        },\n" +
            "        {\n" +
            "          \"conditionsTopGroup\": {\n" +
            "            \"operator\": \"AND\",\n" +
            "            \"conditionsGroups\": [\n" +
            "              {\n" +
            "                \"operator\": \"OR\",\n" +
            "                \"conditions\": [\n" +
            "                  {\n" +
            "                    \"property\": \"id\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"EQUALS_NONE\",\n" +
            "                      \"values\": [\n" +
            "                        1,\n" +
            "                        2,\n" +
            "                        3\n" +
            "                      ]\n" +
            "                    }\n" +
            "                  },\n" +
            "                  {\n" +
            "                    \"property\": \"id\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"EQUALS_NONE\",\n" +
            "                      \"values\": [\n" +
            "                        4\n" +
            "                      ]\n" +
            "                    }\n" +
            "                  }\n" +
            "                ]\n" +
            "              },\n" +
            "              {\n" +
            "                \"operator\": \"AND\",\n" +
            "                \"conditions\": [\n" +
            "                  {\n" +
            "                    \"property\": \"id\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"EQUALS_ANY\",\n" +
            "                      \"values\": [\n" +
            "                        1\n" +
            "                      ]\n" +
            "                    }\n" +
            "                  }\n" +
            "                ]\n" +
            "              }\n" +
            "            ]\n" +
            "          }\n" +
            "        }\n" +
            "      ],\n" +
            "      \"articleList_name\": [\n" +
            "        {\n" +
            "          \"condition\": {\n" +
            "            \"property\": \"articleArray_name\",\n" +
            "            \"constraint\": {\n" +
            "              \"type\": \"EQUALS_NULL\"\n" +
            "            }\n" +
            "          }\n" +
            "        }\n" +
            "      ],\n" +
            "      \"someInt\": [\n" +
            "        {\n" +
            "          \"conditionsGroup\": {\n" +
            "            \"operator\": \"AND\",\n" +
            "            \"conditions\": [\n" +
            "              {\n" +
            "                \"property\": \"someString\",\n" +
            "                \"constraint\": {\n" +
            "                  \"type\": \"REGEX_ANY\",\n" +
            "                  \"values\": [\n" +
            "                    \"nomatch\"\n" +
            "                  ]\n" +
            "                }\n" +
            "              },\n" +
            "              {\n" +
            "                \"property\": \"status\",\n" +
            "                \"constraint\": {\n" +
            "                  \"type\": \"REGEX_ANY\",\n" +
            "                  \"values\": [\n" +
            "                    \"E\"\n" +
            "                  ]\n" +
            "                }\n" +
            "              },\n" +
            "              {\n" +
            "                \"property\": \"startDate\",\n" +
            "                \"constraint\": {\n" +
            "                  \"type\": \"DATE_FUTURE\",\n" +
            "                  \"days\": 0\n" +
            "                }\n" +
            "              },\n" +
            "              {\n" +
            "                \"property\": \"startLocalDate\",\n" +
            "                \"constraint\": {\n" +
            "                  \"type\": \"DATE_PAST\",\n" +
            "                  \"days\": 2\n" +
            "                }\n" +
            "              },\n" +
            "              {\n" +
            "                \"property\": \"startCalDate\",\n" +
            "                \"constraint\": {\n" +
            "                  \"type\": \"DATE_FUTURE\",\n" +
            "                  \"days\": 100\n" +
            "                }\n" +
            "              }\n" +
            "            ]\n" +
            "          }\n" +
            "        }\n" +
            "      ],\n" +
            "      \"status\": [\n" +
            "        {\n" +
            "          \"conditionsTopGroup\": {\n" +
            "            \"operator\": \"OR\",\n" +
            "            \"conditionsGroups\": [\n" +
            "              {\n" +
            "                \"operator\": \"AND\",\n" +
            "                \"conditions\": [\n" +
            "                  {\n" +
            "                    \"property\": \"someInt\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"RANGE\",\n" +
            "                      \"min\": 1,\n" +
            "                      \"max\": 999\n" +
            "                    }\n" +
            "                  },\n" +
            "                  {\n" +
            "                    \"property\": \"someInt\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"RANGE\",\n" +
            "                      \"max\": 999\n" +
            "                    }\n" +
            "                  },\n" +
            "                  {\n" +
            "                    \"property\": \"someLong\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"RANGE\",\n" +
            "                      \"max\": 9007199\n" +
            "                    }\n" +
            "                  },\n" +
            "                  {\n" +
            "                    \"property\": \"aBoolean\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"EQUALS_ANY\",\n" +
            "                      \"values\": [\n" +
            "                        true\n" +
            "                      ]\n" +
            "                    }\n" +
            "                  },\n" +
            "                  {\n" +
            "                    \"property\": \"someInt\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"EQUALS_NOT_NULL\"\n" +
            "                    }\n" +
            "                  },\n" +
            "                  {\n" +
            "                    \"property\": \"id\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"EQUALS_NONE\",\n" +
            "                      \"values\": [\n" +
            "                        1,\n" +
            "                        123456789\n" +
            "                      ]\n" +
            "                    }\n" +
            "                  }\n" +
            "                ]\n" +
            "              },\n" +
            "              {\n" +
            "                \"operator\": \"AND\",\n" +
            "                \"conditions\": [\n" +
            "                  {\n" +
            "                    \"property\": \"id\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"EQUALS_NONE\",\n" +
            "                      \"values\": [\n" +
            "                        666,\n" +
            "                        999\n" +
            "                      ]\n" +
            "                    }\n" +
            "                  }\n" +
            "                ]\n" +
            "              }\n" +
            "            ]\n" +
            "          }\n" +
            "        }\n" +
            "      ],\n" +
            "      \"aBoolean\": [\n" +
            "        {\n" +
            "          \"conditionsTopGroup\": {\n" +
            "            \"operator\": \"AND\",\n" +
            "            \"conditionsGroups\": [\n" +
            "              {\n" +
            "                \"operator\": \"OR\",\n" +
            "                \"conditions\": [\n" +
            "                  {\n" +
            "                    \"property\": \"someString\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"SIZE\",\n" +
            "                      \"min\": 1,\n" +
            "                      \"max\": 100\n" +
            "                    }\n" +
            "                  },\n" +
            "                  {\n" +
            "                    \"property\": \"articleList\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"SIZE\",\n" +
            "                      \"min\": 1\n" +
            "                    }\n" +
            "                  },\n" +
            "                  {\n" +
            "                    \"property\": \"articleArray\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"SIZE\",\n" +
            "                      \"max\": 100\n" +
            "                    }\n" +
            "                  }\n" +
            "                ]\n" +
            "              },\n" +
            "              {\n" +
            "                \"operator\": \"OR\",\n" +
            "                \"conditions\": [\n" +
            "                  {\n" +
            "                    \"property\": \"id\",\n" +
            "                    \"constraint\": {\n" +
            "                      \"type\": \"EQUALS_NONE\",\n" +
            "                      \"values\": [\n" +
            "                        404\n" +
            "                      ]\n" +
            "                    }\n" +
            "                  }\n" +
            "                ]\n" +
            "              }\n" +
            "            ]\n" +
            "          }\n" +
            "        }\n" +
            "      ],\n" +
            "      \"customer_name\": [\n" +
            "        {\n" +
            "          \"conditionsGroup\": {\n" +
            "            \"operator\": \"AND\",\n" +
            "            \"conditions\": [\n" +
            "              {\n" +
            "                \"property\": \"status\",\n" +
            "                \"constraint\": {\n" +
            "                  \"type\": \"EQUALS_ANY\",\n" +
            "                  \"values\": [\n" +
            "                    \"NEW\"\n" +
            "                  ]\n" +
            "                }\n" +
            "              },\n" +
            "              {\n" +
            "                \"property\": \"status\",\n" +
            "                \"constraint\": {\n" +
            "                  \"type\": \"EQUALS_NOT_NULL\"\n" +
            "                }\n" +
            "              }\n" +
            "            ]\n" +
            "          }\n" +
            "        }\n" +
            "      ]\n" +
            "    }\n" +
            "  }\n" +
            "}\n";

    private static final String JSON_WS = "[ \\u0009\\u000d\\u000a]*";
    private static final Pattern BIG_INT_PATTERN = Pattern.compile("^-?\\d+$");

    // TODO check keys are unique
    public static void main(String[] args) {
        ConsumerTesting consumerTesting = new ConsumerTesting();
        System.out.println("result: " + consumerTesting.parseJson("\t 'foo'  ".replace('\'', '"')));
        System.out.println("result: " + consumerTesting.parseJson("   'foo' \t should fail ".replace('\'', '"')));
        System.out.println("result: " + consumerTesting.parseJson(" 123 ".replace('\'', '"')));
        System.out.println("result: " + consumerTesting.parseJson(" true ".replace('\'', '"')));
        System.out.println("result: " + consumerTesting.parseJson(" false ".replace('\'', '"')));
        System.out.println("result: " + consumerTesting.parseJson(" {} ".replace('\'', '"')));
        System.out.println("result: " + consumerTesting.parseJson(" {} should fail ".replace('\'', '"')));
        System.out.println("result: " + consumerTesting.parseJson(" {'foo':true} ".replace('\'', '"')));
        System.out.println("result: " + consumerTesting.parseJson(" [] ".replace('\'', '"')));
        System.out.println("result: " + consumerTesting.parseJson("['two',456,false,null]".replace('\'', '"')));
        System.out.println("result: " + consumerTesting.parseJson("['three',{},{'foo':[1,2,3]}]".replace('\'', '"')));
        Object jsonObject = consumerTesting.parseJson(testJson);
        System.out.println("result: " + jsonObject);
        long ts1 = System.nanoTime();
        Object jsonRules = consumerTesting.parseJson(testJson2);
        System.out.println("result: " + jsonRules);
        long ts2 = System.nanoTime();
        System.out.println("time ms: " + (ts2 -ts1)/1000/1000);

        mapJsonObjectToEntity(jsonRules);
    }

    private static void mapJsonObjectToEntity(Object jsonRules) {
        Map<String, Object> root = getObjectAsMapOrFail(jsonRules, "root");
        System.out.println("schema-version: "+ root.get("schema-version"));
        Map<String, Object> mandatoryRules = getElementAsMapOrFail("mandatoryRules", root);
        System.out.println("types: " + mandatoryRules.keySet());
        mandatoryRules.keySet().stream()
                .peek(type -> System.out.println("type: " + type))
                .map(type -> getObjectAsMapOrFail(mandatoryRules.get(type), "mandatoryRules"))
                .forEach(propMap -> System.out.println("properties: " + propMap.keySet()));
        // etc. pp.
    }

    private static Map<String, Object> getElementAsMapOrFail(String key, Map<String, Object> map) {
        return getObjectAsMapOrFail(map.get(key), key);
    }

    private static Map<String, Object> getObjectAsMapOrFail(Object object, String keyToLog) {
        if (object == null) {
            throw new IllegalArgumentException("object not found for: " + keyToLog);
        }
        if (!(object instanceof Map)) {
            throw new IllegalArgumentException("object should be of type map for: " + keyToLog + ", but is: " +
                    object.getClass().getSimpleName());
        }
        return (Map) object;
    }


    public Object parseJson(String json) {
        if (json == null || json.replaceFirst(JSON_WS, "").isEmpty())
            return new IllegalArgumentException("Invalid JSON: Expecting 'STRING', 'NUMBER', 'NULL', 'TRUE', 'FALSE', '{', '[', got 'EOF'");
        ParseData parseData = parseElement(json.replaceFirst(JSON_WS, ""), "root");
        String jsonTail = parseData.getJsonTail().replaceFirst(JSON_WS, "");
        if (!jsonTail.isEmpty())
            return new IllegalArgumentException("Invalid JSON: Expecting 'EOF', got '" + jsonTail + "'");
        return parseData.getJsonObject();
    }

    private ParseData parseElement(String json, String nameOfObjectToLog) {
        if (json.replaceAll(JSON_WS, "").isEmpty())
            return new ParseData(null, "");

        TokenData token = extractNextToken(json, nameOfObjectToLog);

        switch (token.getTokenType()) {
        case STRING:
            return new ParseData(token.getTokenText(), token.jsonTail);
        case NUMBER:
            return new ParseData(convertToNumber(token.getTokenText()), token.jsonTail);
        case BOOLEAN:
            return new ParseData(Boolean.valueOf(token.getTokenText()), token.jsonTail);
        case NULL:
            return new ParseData(null, token.jsonTail);
        case OBJECT_START:
            return parseObject(token.getJsonTail(), nameOfObjectToLog);
        case ARRAY_START:
            return parseArray(token.getJsonTail(), nameOfObjectToLog);
        default:
            throw new IllegalArgumentException("Invalid JSON at '" + nameOfObjectToLog + "': " + token.getTokenText());
        }
    }

    private Object convertToNumber(String number) {
        if (BIG_INT_PATTERN.matcher(number).matches()) {
            return new BigInteger(number);
        }
        return new BigDecimal(number);
    }

    private ParseData parseObject(String jsonTail, String nameOfObjectToLog) {
        Map<String, Object> map = new LinkedHashMap<>(); // keep insertion order

        TokenData nextToken = extractNextToken(jsonTail, nameOfObjectToLog);
        if (nextToken.getTokenType() == TokenType.OBJECT_END) {
            return new ParseData(map, nextToken.getJsonTail());
        }
        TokenData keyToken = nextToken;
        TokenData endingToken;
        do {
            if (keyToken.getTokenType() != TokenType.STRING) {
                throw new IllegalArgumentException("Invalid JSON at '" + keyToken.getTokenText() + "': Expecting " +
                        "'STRING', '}', got '" + keyToken.getTokenType() + "'");
            }
            String key = keyToken.getTokenText();
            TokenData colonToken = extractNextToken(keyToken.getJsonTail(), key);
            if (colonToken.getTokenType() != TokenType.COLON) {
                throw new IllegalArgumentException("Invalid JSON at '" + keyToken.getTokenText() + "': Expecting ':', " +
                        "got '" + colonToken.getTokenType() + "'");
            }

            ParseData parseData = parseElement(colonToken.getJsonTail(), key);
            map.put(key, parseData.jsonObject);

            endingToken = extractNextToken(parseData.getJsonTail(), key);
            if (endingToken.getTokenType() == TokenType.COMMA) {
                keyToken = extractNextToken(endingToken.getJsonTail(), key);
            }
        } while (endingToken.getTokenType() == TokenType.COMMA);

        if (endingToken.getTokenType() != TokenType.OBJECT_END) {
            throw new IllegalArgumentException("Invalid JSON at '" + keyToken.getTokenText() + "': Expecting " +
                    "'}', ',' , got '" + keyToken.getTokenType() + "'");
        }
        return new ParseData(map, endingToken.getJsonTail());
    }

    private ParseData parseArray(String jsonTail, String nameOfObjectToLog) {
        ArrayList<Object> array = new ArrayList<>();

        TokenData nextToken = extractNextToken(jsonTail, nameOfObjectToLog);
        if (nextToken.getTokenType() == TokenType.ARRAY_END) {
            return new ParseData(array, nextToken.getJsonTail());
        }
        TokenData endingToken;
        String endingJsonTail = jsonTail;
        do {
            ParseData parseData = parseElement(endingJsonTail, "[");
            array.add(parseData.getJsonObject());
            endingToken = extractNextToken(parseData.getJsonTail(), nameOfObjectToLog);
            if (endingToken.getTokenType() == TokenType.COMMA) {
                endingJsonTail = endingToken.getJsonTail();
            }
        } while (endingToken.getTokenType() == TokenType.COMMA);

        if (endingToken.getTokenType() != TokenType.ARRAY_END) {
            throw new IllegalArgumentException("Invalid JSON at '" + endingToken.getTokenText() + "': Expecting " +
                    "']', ',', got '" + endingToken.getTokenType() + "'");
        }
        return new ParseData(array, endingToken.getJsonTail());
    }

    private TokenData extractNextToken(String json, String nameOfObjectToLog) {
        return extractTokenOfType(TokenType.STRING, json)
                .orElseGet(() -> extractTokenOfType(TokenType.NUMBER, json)
                .orElseGet(() -> extractTokenOfType(TokenType.BOOLEAN, json)
                .orElseGet(() -> extractTokenOfType(TokenType.OBJECT_START, json)
                .orElseGet(() -> extractTokenOfType(TokenType.OBJECT_END, json)
                .orElseGet(() -> extractTokenOfType(TokenType.ARRAY_START, json)
                .orElseGet(() -> extractTokenOfType(TokenType.ARRAY_END, json)
                .orElseGet(() -> extractTokenOfType(TokenType.COLON, json)
                .orElseGet(() -> extractTokenOfType(TokenType.COMMA, json)
                .orElseGet(() -> extractTokenOfType(TokenType.NULL, json)
                .orElseThrow(() -> new IllegalArgumentException("Invalid JSON near '" + nameOfObjectToLog +
                        "' Expecting 'STRING', 'NUMBER', 'NULL', 'TRUE', 'FALSE', '{', '[', got: " +
                        extractTokenOfType(TokenType.INVALID, json).get().tokenText)))))))))));
    }

    private Optional<TokenData> extractTokenOfType(TokenType tokenType,String json) {
        Matcher matcher = tokenType.getPattern().matcher(json);
        if (matcher.find()) {
            String rest = json.replaceFirst(tokenType.getPattern().pattern(), "");
            //System.out.println("Token: " + tokenType + " " + matcher.group(1) + ",  rest: " + rest);
            return Optional.of(new TokenData(tokenType, matcher.group(1), rest));
        }
        return Optional.empty();
    }


    private static class ParseData {
        private final Object jsonObject;
        private final String jsonTail;

        public ParseData(Object jsonObject, String jsonTail) {
            this.jsonObject = jsonObject;
            this.jsonTail = jsonTail;
        }

        public Object getJsonObject() {
            return jsonObject;
        }

        public String getJsonTail() {
            return jsonTail;
        }
    }

    private enum TokenType {
        // Regex see https://stackoverflow.com/questions/2583472/regex-to-validate-json
        STRING("^\"(([^\"\\\\]*|\\\\[\"\\\\bfnrt\\/]|\\\\u[0-9a-f]{4})*)\""),
        NUMBER("^(-?(?=[1-9]|0(?!\\d))\\d+(\\.\\d+)?([eE][+-]?\\d+)?)"),
        BOOLEAN("^(true|false)"),
        NULL("^(null)"),
        OBJECT_START("^(\\{)"),
        OBJECT_END("^(})"),
        ARRAY_START("^(\\[)"),
        ARRAY_END("^(])"),
        COLON("^(:)"),
        COMMA("^(,)"),
        INVALID("^((.?)*)");

        private Pattern pattern;

        TokenType(String regex) {
            this.pattern = Pattern.compile(regex + JSON_WS);
        }

        public Pattern getPattern() {
            return pattern;
        }
    }

    private class TokenData {
        private TokenType tokenType;
        private String tokenText;
        private String jsonTail;

        public TokenData(TokenType tokenType, String tokenText, String jsonTail) {
            this.tokenType = tokenType;
            this.tokenText = tokenText;
            this.jsonTail = jsonTail;
        }

        public TokenType getTokenType() {
            return tokenType;
        }

        public String getTokenText() {
            return tokenText;
        }

        public String getJsonTail() {
            return jsonTail;
        }
    }

}
