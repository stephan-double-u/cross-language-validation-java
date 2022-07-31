package de.swa.clv;

import de.swa.clv.constraints.ValidationRule;
import de.swa.clv.json.JsonSerializable;

import java.util.*;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.*;

/**
 * Maps property names to their validation rules ({@code List&lt;ValidationRule&gt;}).
 * A {@link LinkedHashMap} is used to preserve insertion order and thereby define the validation order.
 * This can be used to e.g. do cheaper validations first.
 */
public class PropertyRulesMap implements JsonSerializable {

    // Linked HashMap to preserve insertion order and thereby define validation order, e.g. to do cheaper validations
    // first
    private final Map<String, List<ValidationRule>> map = new LinkedHashMap<>();

    public Set<String> keySet() {
        return map.keySet();
    }

    public List<ValidationRule> get(final String property) {
        return map.get(property);
    }

    public List<ValidationRule> getOrInit(final String property) {
        return map.computeIfAbsent(property, key -> new ArrayList<>());
    }

    @Override
    public String serializeToJson() {
        // serialize only if at least one rule is not flagged as 'doNotSerialize'
        return map.entrySet().stream()
                .filter(propRule -> propRule.getValue().stream()
                        .anyMatch(rule -> !rule.isDoNotSerialize()))
                .map(propRule -> asKey(propRule.getKey()) + asArray(serialize(propRule.getValue())))
                .collect(Collectors.joining(","));
    }

    private String serialize(List<ValidationRule> validationRuleList) {
        String joinedJson = validationRuleList.stream()
                .map(ValidationRule::serializeToJson)
                .filter(json -> !json.isEmpty())
                .collect(Collectors.joining(","));
        return "{}".equals(joinedJson) ? "" : joinedJson;
    }

}
