package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.Permissions;
import de.swa.easyvalidation.groups.ConstraintRefTopGroup;
import de.swa.easyvalidation.json.JsonSerializable;
import de.swa.easyvalidation.json.JsonUtil;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static de.swa.easyvalidation.json.JsonUtil.asObject;

/**
 * Maps {@code Permissions} to {@code ConstraintRefTopGroup}.
 */
public class PermissionsMap implements JsonSerializable {
    // Linked HashMap to preserve insertion order ??
    private final Map<Permissions, ConstraintRefTopGroup> map = new LinkedHashMap<>();

    public Set<Permissions> getKeys() {
        return map.keySet();
    }

    public ConstraintRefTopGroup getValue(final Permissions permissions) {
        return map.get(permissions);
    }

    @Override
    public String serializeToJson() {
        final String mapAsJson = map.entrySet().stream()
                .map(e -> e.getKey().serializeToJson() + e.getValue().serializeToJson())
                .collect(Collectors.joining(","));
        return mapAsJson;
    }

}
