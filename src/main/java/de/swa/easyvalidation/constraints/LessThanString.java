package de.swa.easyvalidation.constraints;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Comparator;

public class LessThanString extends LessThan {

    private static Logger log = LoggerFactory.getLogger(LessThanString.class);

    public LessThanString comparator(Comparator<String> comparator) {
        setComparator(comparator);
        return this;
    }
    
}
