package de.swa.clv.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class IndexedPropertyHelper {
    
    private IndexedPropertyHelper() {
    }

    public static final String INVALID_INDEXED_PROPERTY_MESSAGE = "Not a valid indexed property: ";

    // Regular expression for valid index definitions
    private static final String IDX_REG_EXP = "^(\\d+(,\\d+)*+|\\d+/\\d+|\\d+-\\d+$|\\*)$";
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
     * To sum it up: the content_ between the brackets must match: "\d+(,\d+)*+$|^\d+\/\d$|^\d+-\d+$|^\*$"<br/>
     * Values are NOT sorted and duplicate values are NOT removed.<br/>
     * You are right, spaces are not allowed.
     *
     * @param property property that may contain index definitions
     * @return an optional IndexInfo
     */
    public static Optional<IndexInfo> getIndexInfo(final String property) {
        if (!isIndexedProperty(property)) {
            return Optional.empty();
        }
        propertyToIndexInfoCache.putIfAbsent(property, createIndexInfo(property));
        return Optional.of(propertyToIndexInfoCache.get(property));
    }

    private static IndexInfo createIndexInfo(final String indexedProperty) {
        // Check for valid bracket positions
        final int startPos = indexedProperty.lastIndexOf('[');
        final int endPos = indexedProperty.indexOf(']', startPos);
        if (endPos - startPos < 2 || indexedProperty.length() -1 > endPos) {
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
            return getIncrementIndexInfo(indexStr, indexedProperty);
        }
        // Handle 'enumeration' and 'interval' definitions
        return getListIndexInfo(indexedProperty, indexStr);
    }

    private static IndexInfo getListIndexInfo(String indexedProperty, String indexStr) {
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
                final int index = Integer.parseInt(indexPart);
                if (index < 0) {
                    throw new IllegalArgumentException(INVALID_INDEXED_PROPERTY_MESSAGE + indexedProperty);
                }
                values.add(index);
            }
        }
        return new IndexInfo(IndexType.LIST, values);
    }

    private static IndexInfo getIncrementIndexInfo(String indexStr, String indexedPropertyToLog) {
        final String[] startIncr = indexStr.split("/");
        final int startIndex = Integer.parseInt(startIncr[0]);
        final int increment = Integer.parseInt(startIncr[1]);
        if (startIndex < 0 || increment < 1) {
            throw new IllegalArgumentException(INVALID_INDEXED_PROPERTY_MESSAGE + indexedPropertyToLog);
        }
        return new IndexInfo(IndexType.INCREMENT, Arrays.asList(startIndex, increment));
    }

    public enum IndexType {
        LIST, INCREMENT
    }

    public record IndexInfo (IndexType indexType, List<Integer> values) {
    }

}
