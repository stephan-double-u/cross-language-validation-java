package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.Permissions;
import de.swa.easyvalidation.groups.ConstraintRefTopGroup;
import de.swa.easyvalidation.json.JsonSerializable;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static de.swa.easyvalidation.json.JsonUtil.asKey;

public class PropertyMap implements JsonSerializable {
    // <property> -> {<permissions> -> ConstraintRefTopGroup}
    // Linked HashMap to preserve insertion order and thereby define validation order, e.g. to do cheap validations first!
    // -> no need for javax.validation.GroupSequence!

    //TODO! Use PermissionsMap instead of Map<Permissions, ConstraintRefTopGroup>
    private final Map<String, Map<Permissions, ConstraintRefTopGroup>> map = new LinkedHashMap<>();
    private final Map<String, PermissionsMap> map__ = new LinkedHashMap<>();

    public Set<String> getKeys() {
        return map__.keySet();
    }

    public PermissionsMap getValueOrInit(final String property) {
        if (map__.get(property) == null) {
            map__.put(property, new PermissionsMap());
        }
        return map__.get(property);
    }

    public Map<Permissions, ConstraintRefTopGroup> getOrInitPermissionsMap(final String property) {
        if (map.get(property) == null) {
            map.put(property, new LinkedHashMap<>());
        }
        return map.get(property);
    }

    public PermissionsMap getValue(final String property) {
        return map__.get(property);
    }

    public Map<Permissions, ConstraintRefTopGroup> getPermissionsMap(final String property) {
        return map.get(property);
    }

    public String serializeToJson__() {
        final String mapAsJson = map__.entrySet().stream()
                .map(e -> asKey(e.getKey()) + e.getValue().serializeToJson())
                .collect(Collectors.joining(","));
        return mapAsJson;
    }
    @Override
    public String serializeToJson() {
        final String mapAsJson = map.entrySet().stream()
                .map(e -> asKey(e.getKey()) + serialize(e.getValue()))
                .collect(Collectors.joining(","));
        return mapAsJson;
    }

    private String serialize(Map<Permissions, ConstraintRefTopGroup> permissionsMap) {
        if (permissionsMap.isEmpty()) {
            return "[]"; //TODO
        }
        return "TODO";
    }
}
