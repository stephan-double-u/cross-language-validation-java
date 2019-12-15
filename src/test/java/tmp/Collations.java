package tmp;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.Collator;
import java.util.Locale;

public class Collations {

    private static Logger log = LoggerFactory.getLogger(Collations.class);

    public static void main(String[] args) {
        /* Gemeinsamkeiten und Unterschiede zw. java.text.Collator und Javascript String#localeCompare() ?!
         * 
         * String.compareTo: lexicographically based on the Unicode value
         * String.CASE_INSENSITIVE_ORDER: Comparator w/o locales support ...
         * -> Collator#compare
         */
        for (Locale loc : Collator.getAvailableLocales()) {
            //log.debug(loc);
        }
        Collator collatorDE = Collator.getInstance(Locale.GERMANY);
        Collator collatorSE = Collator.getInstance(new Locale("sv", "SE"));
        log.debug("" + collatorDE.compare("ä", "z")); // -1
        log.debug("" + collatorSE.compare("ä", "z")); // 1 !
        log.debug("" + collatorDE.compare("ä", "a")); // 1
        collatorDE.setStrength(Collator.PRIMARY);
        log.debug("" + collatorDE.compare("ä", "a")); // 0 !
        
    }

}
