package de.swa.easyvalidation.constraints;

import java.util.Comparator;

public class LessThanString extends LessThan {

    public LessThanString comparator(final Comparator<String> comparator) {
        setComparator(comparator);
        return this;
    }
    
}
