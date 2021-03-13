package de.swa.clv;

import de.swa.clv.json.JsonSerializable;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.asArray;
import static de.swa.clv.json.JsonUtil.asKey;

/**
 * Maps property string to {@code ContentPermissionsMap}.
 */
public class ContentPropertyMap implements JsonSerializable {

    // Linked HashMap to preserve insertion order and thereby define validation order, e.g. to do cheap validations first,
    // i.e. no need for something like javax.validation.GroupSequence
    private final Map<String, ContentPermissionsMap> map = new LinkedHashMap<>();

    public Set<String> keySet() {
        return map.keySet();
    }

    public ContentPermissionsMap get(final String property) {
        return map.get(property);
    }

    public ContentPermissionsMap getOrInit(final String property) {
        return map.computeIfAbsent(property, key -> new ContentPermissionsMap());
    }

    @Override
    public String serializeToJson() {
        return map.entrySet().stream()
                .map(e -> asKey(e.getKey()) + asArray(e.getValue().serializeToJson()))
                .collect(Collectors.joining(","));
    }
}
