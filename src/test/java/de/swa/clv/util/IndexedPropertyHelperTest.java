package de.swa.clv.util;

import org.hamcrest.core.StringContains;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.Arrays;

import static org.junit.Assert.*;

public class IndexedPropertyHelperTest {

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Test
    public void exceptionIfInvalidIndexDefinition() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Not a valid indexed property"));
        IndexedPropertyHelper.getIndexInfo("[3-1,xxx]");
    }

    @Test
    public void exceptionIfListToValueIsLessThanFromValue() {
        expectedEx.expect(IllegalArgumentException.class);
        expectedEx.expectMessage(StringContains.containsString("Not a valid indexed property"));
        IndexedPropertyHelper.getIndexInfo("[3-1]");
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

    @Test
    public void getIndexInfo6() {
        IndexedPropertyHelper.IndexInfo info = new IndexedPropertyHelper.IndexInfo(IndexedPropertyHelper.IndexType.LIST, Arrays.asList(1, 2, 3, 2, 3, 4));
        assertEquals(info, IndexedPropertyHelper.getIndexInfo("foo[1-3,2,3-4]").get());
    }
}