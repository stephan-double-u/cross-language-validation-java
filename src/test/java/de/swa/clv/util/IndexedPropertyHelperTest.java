package de.swa.clv.util;

import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

class IndexedPropertyHelperTest {

    @Test
    void exceptionIfInvalidIndexDefinition() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> IndexedPropertyHelper.getIndexInfo("[3-1,xxx]"));
        assertEquals("Not a valid indexed property: [3-1,xxx]", ex.getMessage());
    }

    @Test
    void exceptionIfListToValueIsLessThanFromValue() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> IndexedPropertyHelper.getIndexInfo("[3-1]"));
        assertEquals("Not a valid indexed property: [3-1]", ex.getMessage());
    }

    @Test
    void exceptionIfListAndRange() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> IndexedPropertyHelper.getIndexInfo("[1,2-3]"));
        assertEquals("Not a valid indexed property: [1,2-3]", ex.getMessage());
    }

    @Test
    void isIndexedProperty() {
        assertTrue(IndexedPropertyHelper.isIndexedProperty("foo[*]"));
    }

    @Test
    void getIndexInfo1() {
        IndexedPropertyHelper.IndexInfo info = new IndexedPropertyHelper.IndexInfo(IndexedPropertyHelper.IndexType.INCREMENT, Arrays.asList(0, 1));
        assertEquals(info, IndexedPropertyHelper.getIndexInfo("foo[*]").get());
    }

    @Test
    void getIndexInfo2() {
        IndexedPropertyHelper.IndexInfo info = new IndexedPropertyHelper.IndexInfo(IndexedPropertyHelper.IndexType.LIST, Arrays.asList(2));
        assertEquals(info, IndexedPropertyHelper.getIndexInfo("foo[2]").get());
    }

    @Test
    void getIndexInfo3() {
        IndexedPropertyHelper.IndexInfo info = new IndexedPropertyHelper.IndexInfo(IndexedPropertyHelper.IndexType.LIST, Arrays.asList(1, 2, 3));
        assertEquals(info, IndexedPropertyHelper.getIndexInfo("foo[1-3]").get());
    }

    @Test
    void getIndexInfo4() {
        IndexedPropertyHelper.IndexInfo info = new IndexedPropertyHelper.IndexInfo(IndexedPropertyHelper.IndexType.LIST, Arrays.asList(1 ,3 ,5));
        assertEquals(info, IndexedPropertyHelper.getIndexInfo("foo[1,3,5]").get());
    }

    @Test
    void getIndexInfo5() {
        IndexedPropertyHelper.IndexInfo info = new IndexedPropertyHelper.IndexInfo(IndexedPropertyHelper.IndexType.INCREMENT, Arrays.asList(3, 2));
        assertEquals(info, IndexedPropertyHelper.getIndexInfo("foo[3/2]").get());
    }

}
