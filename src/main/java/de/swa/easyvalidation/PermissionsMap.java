package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.Permissions;
import de.swa.easyvalidation.groups.RelationsTopGroup;
import de.swa.easyvalidation.json.JsonSerializable;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static de.swa.easyvalidation.json.JsonUtil.asObject;

/**
 * Maps {@code Permissions} to {@code RelationsTopGroup}.
 */
public class PermissionsMap implements JsonSerializable {
    // Linked HashMap to preserve insertion order ?!
    private final Map<Permissions, RelationsTopGroup> map = new LinkedHashMap<>();

    public Set<Permissions> keySet() {
        return map.keySet();
    }

    public RelationsTopGroup get(final Permissions permissions) {
        return map.get(permissions);
    }

    public Set<Map.Entry<Permissions, RelationsTopGroup>> entrySet() {
        return map.entrySet();
    }

    public void put(Permissions permissions, RelationsTopGroup topGroup) {
        map.put(permissions, topGroup);
    }

    @Override
    public String serializeToJson() {
        final String mapAsJson = map.entrySet().stream()
                .map(e -> serialize(e))
                .collect(Collectors.joining(","));
        return mapAsJson;
    }

    private String serialize(Map.Entry<Permissions, RelationsTopGroup> e) {
        final String key = e.getKey().serializeToJson();
        final String val = e.getValue().serializeToJson();
        final String sep = key.isEmpty() || val.isEmpty() ? "" : ",";
        String json = key + sep + val;
        return !json.isEmpty() ? asObject(json) : "";
    }

}
