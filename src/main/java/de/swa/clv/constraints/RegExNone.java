package de.swa.clv.constraints;

import de.swa.clv.util.TypeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;

import static de.swa.clv.json.JsonUtil.*;

public class RegExNone extends RegEx {

    static final String TYPE = "REGEX_NONE";

    RegExNone(String... regex) {
        super(regex);
    }

    @Override
    public String getType() {
        return TYPE;
    }
}
