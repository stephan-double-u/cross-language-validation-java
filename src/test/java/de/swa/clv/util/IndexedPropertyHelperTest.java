package de.swa.clv.util;

import org.hamcrest.core.StringContains;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.*;

public class IndexedPropertyHelperTest {

    @Test
    public void exceptionIfInvalidIndexDefinition() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> IndexedPropertyHelper.getIndexInfo("[3-1,xxx]"));
        assertEquals("Not a valid indexed property: [3-1,xxx]", ex.getMessage());
    }

    @Test
    public void exceptionIfListToValueIsLessThanFromValue() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> IndexedPropertyHelper.getIndexInfo("[3-1]"));
        assertEquals("Not a valid indexed property: [3-1]", ex.getMessage());
    }

    @Test
    public void exceptionIfListAndRange() {
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> IndexedPropertyHelper.getIndexInfo("[1,2-3]"));
        assertEquals("Not a valid indexed property: [1,2-3]", ex.getMessage());
    }

    @Test
    public void isIndexedProperty() {
        assertTrue(IndexedPropertyHelper.isIndexedProperty("foo[*]"));
    }

    @Test
    public void getIndexInfo1() {
        IndexedPropertyHelper.IndexInfo info = new IndexedPropertyHelper.IndexInfo(IndexedPropertyHelper.IndexType.INCREMENT, Arrays.asList(0, 1));
        assertEquals(info, IndexedPropertyHelper.getIndexInfo("foo[*]").get());
    }

    @Test
    public void getIndexInfo2() {
        IndexedPropertyHelper.IndexInfo info = new IndexedPropertyHelper.IndexInfo(IndexedPropertyHelper.IndexType.LIST, Arrays.asList(2));
        assertEquals(info, IndexedPropertyHelper.getIndexInfo("foo[2]").get());
    }

    @Test
    public void getIndexInfo3() {
        IndexedPropertyHelper.IndexInfo info = new IndexedPropertyHelper.IndexInfo(IndexedPropertyHelper.IndexType.LIST, Arrays.asList(1, 2, 3));
        assertEquals(info, IndexedPropertyHelper.getIndexInfo("foo[1-3]").get());
    }

    @Test
    public void getIndexInfo4() {
        IndexedPropertyHelper.IndexInfo info = new IndexedPropertyHelper.IndexInfo(IndexedPropertyHelper.IndexType.LIST, Arrays.asList(1 ,3 ,5));
        assertEquals(info, IndexedPropertyHelper.getIndexInfo("foo[1,3,5]").get());
    }

    @Test
    public void getIndexInfo5() {
        IndexedPropertyHelper.IndexInfo info = new IndexedPropertyHelper.IndexInfo(IndexedPropertyHelper.IndexType.INCREMENT, Arrays.asList(3, 2));
        assertEquals(info, IndexedPropertyHelper.getIndexInfo("foo[3/2]").get());
    }

}
