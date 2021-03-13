package de.swa.clv;

import de.swa.clv.constraints.Conditions;
import de.swa.clv.json.JsonSerializable;

import java.util.*;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.*;

/**
 * Maps property string to {@code List<Conditions>}.
 */
public class PropertyConditionsMap implements JsonSerializable {

    // Linked HashMap to preserve insertion order and thereby define validation order, e.g. to do cheap validations
    // first, i.e. no need for something like javax.validation.GroupSequence
    private final Map<String, List<Conditions>> map = new LinkedHashMap<>();

    public Set<String> keySet() {
        return map.keySet();
    }

    public List<Conditions> get(final String property) {
        return map.get(property);
    }

    public List<Conditions> getOrInit(final String property) {
        return map.computeIfAbsent(property, key -> new ArrayList<>());
    }

    @Override
    public String serializeToJson() {
        return map.entrySet().stream()
                .map(e -> asKey(e.getKey()) + asArray(serialize(e.getValue())))
                .collect(Collectors.joining(","));
    }

    private String serialize(List<Conditions> conditionsList) {
        return conditionsList.stream()
                .map(Conditions::serializeToJson)
                .collect(Collectors.joining(","));
    }

}
