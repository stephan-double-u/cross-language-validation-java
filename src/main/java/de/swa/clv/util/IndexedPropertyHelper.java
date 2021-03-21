package de.swa.clv.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class IndexedPropertyHelper {
    
    private IndexedPropertyHelper() {
    }

    public static final String INVALID_INDEXED_PROPERTY_MESSAGE = "Not a valid indexed property: ";

    // Regular expression for valid index definitions
    //private static final String IDX_REG_EXP = "^\\*$|^\\d+\\/\\d$|^\\d+(-\\d+)?(,\\d+(-\\d+)?)*$";
    private static final String IDX_REG_EXP = "\\d+(,\\d+)*$|^\\d+\\/\\d$|^\\d+-\\d+$|^\\*$";
    private static final Pattern PATTERN = Pattern.compile(IDX_REG_EXP);

    private static final Map<String, IndexInfo> propertyToIndexInfoCache = new HashMap<>();

    public static boolean isIndexedProperty(final String property) {
        return property.contains("]");
    }

    /**
     * Returns a {@code IndexInfo} that contains the {@code IndexType} of the <i>index definition</i> and a list of
     * values for the index definition of the the <b>last</b> property part.<br/>
     * E.g. for the property "foo[*].bar.zoo[0-3]" the second index definition "[0-3]" is processed. The meaning of
     * the values is determined by the {@code IndexType}.<br/>
     * Possible formats and meanings:
     * <ul>
     * <li>[0] : One single index position (value &gt;= 0). {@code IndexType} is {@code LIST}, list contains the index
     * value.</li>
     * <li>[2,4,8] : list of index positions (values &gt;= 0 in arbitrary sequence). {@code IndexType} is
     * {@code LIST}, list contains the listed values.</li>
     * <li>[1-5] : range of index positions (1<sup>st</sup> value &gt;= 0 and 2<sup>nd</sup> value &gt;= 1st value).
     * {@code IndexType} is {@code LIST}, list contains the range values.</li>
     * <li>[0/2] : increment specified index positions (1<sup>st</sup> value &gt;= 0 and 2<sup>nd</sup> value &gt;= 1).
     * {@code IndexType} is {@code INCREMENT}, list contains the start index and the increment.</li>
     * <li>[*] : convenience format for [0/1]
     * </ul>
     * To sum it up: the content_ between the brackets must match: "\d+(,\d+)*$|^\d+\/\d$|^\d+-\d+$|^\*$"<br/>
     * Values are NOT sorted and duplicate values are NOT removed.<br/>
     * You are right, spaces are not allowed.
     *
     * @param indexedProperty
     * @return
     */
    public static Optional<IndexInfo> getIndexInfo(final String indexedProperty) {
        if (!isIndexedProperty(indexedProperty)) {
            return Optional.ofNullable(null);
        }
        if (!propertyToIndexInfoCache.containsKey(indexedProperty)) {
            propertyToIndexInfoCache.put(indexedProperty, createIndexInfo(indexedProperty));
        }
        return Optional.of(propertyToIndexInfoCache.get(indexedProperty));
    }

    private static IndexInfo createIndexInfo(final String indexedProperty) {
        // Check for valid bracket positions
        final int startPos = indexedProperty.lastIndexOf('[');
        final int endPos = indexedProperty.indexOf(']', startPos);
        if (endPos - startPos < 2) {
            throw new IllegalArgumentException(INVALID_INDEXED_PROPERTY_MESSAGE + indexedProperty);
        }
        // Check for valid content within brackets
        final String indexStr = indexedProperty.substring(startPos + 1, endPos);
        final Matcher matcher = PATTERN.matcher(indexStr);
        if (!matcher.matches()) {
            throw new IllegalArgumentException(INVALID_INDEXED_PROPERTY_MESSAGE + indexedProperty);
        }
        // Handle 'all' definition; same as 'increment 0/1'
        if (indexStr.equals("*")) {
            return new IndexInfo(IndexType.INCREMENT, Arrays.asList(0, 1));
        }
        // Handle 'increment' definition
        if (indexStr.contains("/")) {
            final String[] startIncr = indexStr.split("/");
            final int startIndex = Integer.parseInt(startIncr[0]);
            final int increment = Integer.parseInt(startIncr[1]);
            if (startIndex < 0 || increment < 1) {
                throw new IllegalArgumentException(INVALID_INDEXED_PROPERTY_MESSAGE + indexedProperty);
            }
            return new IndexInfo(IndexType.INCREMENT, Arrays.asList(startIndex, increment));
        }
        // Handle 'enumeration' and 'interval' definitions
        final List<Integer> values = new ArrayList<>();
        final String[] indexParts = indexStr.split(",");
        for (final String indexPart : indexParts) {
            if (indexPart.contains("-")) {
                final String[] rangePart = indexPart.split("-");
                final int from = Integer.parseInt(rangePart[0]);
                final int to = Integer.parseInt(rangePart[1]);
                if (from < 0 || to < from) {
                    throw new IllegalArgumentException(INVALID_INDEXED_PROPERTY_MESSAGE + indexedProperty);
                }
                for (int i = from; i <= to; i++) {
                    values.add(i);
                }
            } else {
                final Integer index = Integer.parseInt(indexPart);
                if (index < 0) {
                    throw new IllegalArgumentException(INVALID_INDEXED_PROPERTY_MESSAGE + indexedProperty);
                }
                values.add(index);
            }
        }
        return new IndexInfo(IndexType.LIST, values);
    }


    public enum IndexType {
        LIST, INCREMENT
    }

    public static class IndexInfo {
        private final IndexType indexType;
        private final List<Integer> values;

        public IndexInfo(final IndexType indexType, final List<Integer> values) {
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
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof IndexInfo)) return false;
            IndexInfo indexInfo = (IndexInfo) o;
            return indexType == indexInfo.indexType &&
                    values.equals(indexInfo.values);
        }

        @Override
        public int hashCode() {
            return Objects.hash(indexType, values);
        }

        @Override
        public String toString() {
            return "IndexInfo [indexType=" + indexType + ", values=" + values + "]";
        }
    }

}
