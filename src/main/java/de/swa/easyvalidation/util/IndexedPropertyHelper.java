package de.swa.easyvalidation.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class IndexedPropertyHelper {

    private static Logger log = LoggerFactory.getLogger(IndexedPropertyHelper.class);

    // Regular expression for valid index definitions
    private static final String IDX_REG_EXP = "^\\*$|^\\d+\\/\\d$|^\\d+(-\\d+)?(,\\d+(-\\d+)?)*$";
    private static final Pattern PATTERN = Pattern.compile(IDX_REG_EXP);

    private static Map<String, IndexInfo> propertyToIndexInfoCache = new HashMap<>();

    public static boolean isIndexedProperty(String property) {
        return property.contains("[");
    }

    /**
     * Returns a {@code IndexInfo} that contains the {@code IndexType} of the <i>index definition</i> and a list of
     * values for the <b>last</b> index definition of the property.<br/>
     * E.g. for the property "foo[*].bar.zoo[0-3].baz" the second index definition "[0-3]" is processed. The meaning of
     * the values is determined by the {@code IndexType}.<br/>
     * Possible formats and meanings:
     * <ul>
     * <li>[0] : One single index position (value &gt;= 0). {@code IndexType} is {@code LIST}, list contains the index
     * value.</li>
     * <li>[1-5] : range of index positions (1<sup>st</sup> value &gt;= 0 and 2<sup>nd</sup> value &gt;= 1st value).
     * {@code IndexType} is {@code LIST}, list contains the range values.</li>
     * <li>[2,4,8] : list of index positions (values &gt;= 0 in arbitrary sequence). {@code IndexType} is
     * {@code LIST}, list contains the listed values.</li>
     * <li>[0/2] : increment specified index positions (1<sup>st</sup> value &gt;= 0 and 2<sup>nd</sup> value &gt;= 1).
     * {@code IndexType} is {@code INCREMENT}, list contains the start index and the increment.</li>
     * <li>[*] : convenience format for [0/1]
     * </ul>
     * Range and list definitions can be combined in arbitrary sequence, e.g. [0,7-9,2,3,1-10].<br/>
     * To sum it up: the content between the brackets must match: "^\*$|^\d+\/\d$|^\d+(-\d+)?(,\d+(-\d+)?)*$"<br/>
     * You are right, spaces are not allowed.<br/>
     * Values are NOT sorted and duplicate values are NOT overwritten, i.e. [1-4,2] would result in this list of values:
     * [1,2,3,4,2].<br/>
     * 
     * @param indexedProperty
     * @return
     */
    public static IndexInfo getIndexInfo(String indexedProperty) {
        if (!isIndexedProperty(indexedProperty)) {
            return null;
        }
        if (!propertyToIndexInfoCache.containsKey(indexedProperty)) {
            propertyToIndexInfoCache.put(indexedProperty, createIndexInfo(indexedProperty));
        }
        return propertyToIndexInfoCache.get(indexedProperty);
    }

    private static IndexInfo createIndexInfo(String indexedProperty) {
        // Check for valid bracket positions
        int startPos = indexedProperty.lastIndexOf('[');
        int endPos = indexedProperty.indexOf(']');
        if (endPos - startPos < 2) {
            throw new IllegalArgumentException("Not a valid indexed property: " + indexedProperty);
        }
        // Check for valid content within brackets
        String indexStr = indexedProperty.substring(startPos + 1, endPos);
        Matcher matcher = PATTERN.matcher(indexStr);
        if (!matcher.matches()) {
            throw new IllegalArgumentException("Not a valid indexed property: " + indexedProperty);
        }
        // Handle 'all' definition; same as 'increment 0/1'
        if (indexStr.equals("*")) {
            return new IndexInfo(IndexType.INCREMENT, Arrays.asList(0, 1));
        }
        // Handle 'increment' definition
        if (indexStr.contains("/")) {
            String[] startIncr = indexStr.split("/");
            int startIndex = Integer.valueOf(startIncr[0]);
            int increment = Integer.valueOf(startIncr[1]);
            if (startIndex < 0 || increment < 1) {
                throw new IllegalArgumentException("Not a valid indexed property: " + indexedProperty);
            }
            return new IndexInfo(IndexType.INCREMENT, Arrays.asList(startIndex, increment));
        }
        // Handle 'enumeration' and 'interval' definitions
        List<Integer> values = new ArrayList<>();
        String[] indexParts = indexStr.split(",");
        for (String indexPart : indexParts) {
            if (indexPart.contains("-")) {
                String[] rangePart = indexPart.split("-");
                int from = Integer.valueOf(rangePart[0]);
                int to = Integer.valueOf(rangePart[1]);
                if (from < 0 || to < from) {
                    throw new IllegalArgumentException("Not a valid indexed property: " + indexedProperty);
                }
                for (int i = from; i <= to; i++) {
                    values.add(i);
                }
            } else {
                Integer index = Integer.valueOf(indexPart);
                if (index < 0) {
                    throw new IllegalArgumentException("Not a valid indexed property: " + indexedProperty);
                }
                values.add(index);
            }
        }
        return new IndexInfo(IndexType.LIST, values);
    }


    public static enum IndexType {
        LIST, INCREMENT
    }

    public static class IndexInfo {
        private IndexType indexType;
        private List<Integer> values;

        public IndexInfo(IndexType indexType, List<Integer> values) {
            this.indexType = indexType;
            this.values = values;
        }

        public IndexType getIndexType() {
            return indexType;
        }

        public List<Integer> getValues() {
            return values;
        }

        @Override
        public String toString() {
            return "IndexInfo [indexType=" + indexType + ", values=" + values + "]";
        }
    }

    // TODO -> JUnit test
    public static void main(String[] args) {
        log.debug("" + getIndexInfo("foo[*]"));
        log.debug("" + getIndexInfo("foo[0]"));
        log.debug("" + getIndexInfo("foo[1-1]"));
        log.debug("" + getIndexInfo("foo[1-3]"));
        log.debug("" + getIndexInfo("foo[4,5,6]"));
        log.debug("" + getIndexInfo("foo[0/2]"));
        log.debug("" + getIndexInfo("foo[1,3-6,8,7-7]"));
    }
}
