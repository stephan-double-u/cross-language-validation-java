package de.swa.clv;

import de.swa.clv.constraints.Permissions;
import de.swa.clv.groups.ConditionsTopGroup;
import de.swa.clv.json.JsonSerializable;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.asObject;

/**
 * Maps {@code Permissions} to {@code ConstraintsTopGroup}.
 */
public class PermissionsMap implements JsonSerializable {

    // Linked HashMap to preserve insertion order
    private final Map<Permissions, ConditionsTopGroup> map = new LinkedHashMap<>();

    public Set<Permissions> keySet() {
        return map.keySet();
    }

    public ConditionsTopGroup get(final Permissions permissions) {
        return map.get(permissions);
    }

    public Set<Map.Entry<Permissions, ConditionsTopGroup>> entrySet() {
        return map.entrySet();
    }

    public void put(Permissions permissions, ConditionsTopGroup topGroup) {
        map.put(permissions, topGroup);
    }

    @Override
    public String serializeToJson() {
        return map.entrySet().stream()
                .map(this::serialize)
                .collect(Collectors.joining(","));
    }

    private String serialize(Map.Entry<Permissions, ConditionsTopGroup> e) {
        final String key = e.getKey().serializeToJson();
        final String val = e.getValue().serializeToJson();
        final String sep = key.isEmpty() || val.isEmpty() ? "" : ",";
        String json = key + sep + val;
        return !json.isEmpty() ? asObject(json) : "";
    }

}
