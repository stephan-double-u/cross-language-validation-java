package de.swa.clv;

import de.swa.clv.constraints.Permissions;
import de.swa.clv.groups.ContentConstraints;
import de.swa.clv.json.JsonSerializable;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static de.swa.clv.json.JsonUtil.asKey;
import static de.swa.clv.json.JsonUtil.asObject;

/**
 * Maps {@code Permissions} to {@code ContentConstraints}.
 */
public class ContentPermissionsMap implements JsonSerializable {
    // Linked HashMap to preserve insertion order ?!
    private final Map<Permissions, ContentConstraints> map = new LinkedHashMap<>();

    public Set<Permissions> keySet() {
        return map.keySet();
    }

    public ContentConstraints get(final Permissions permissions) {
        return map.get(permissions);
    }

    public Set<Map.Entry<Permissions, ContentConstraints>> entrySet() {
        return map.entrySet();
    }

    public void put(Permissions permissions, ContentConstraints contentConstraints) {
        map.put(permissions, contentConstraints);
    }

    @Override
    public String serializeToJson() {
        final String mapAsJson = map.entrySet().stream()
                .map(e -> serialize(e))
                .collect(Collectors.joining(","));
        return mapAsJson;
    }

    // Special treatment here: sequence should be <contentConstraint>[,<permsissions>][,<constraintsTopGroup>]
    private String serialize(Map.Entry<Permissions, ContentConstraints> e) {
        final String contentConstraintJson = asKey("contentConstraint") + asObject(e.getValue().getContentConstraint().serializeToJson());
        final String permsissionsJson = e.getKey().serializeToJson();
        final String constraintsTopGroupJson = e.getValue().getConstraintsTopGroup().serializeToJson();
        final String sep1 =  "".equals(permsissionsJson) ? "" : ",";
        final String sep2 = "".equals(constraintsTopGroupJson) ? "" : ",";
        return asObject(contentConstraintJson + sep1 + permsissionsJson + sep2 + constraintsTopGroupJson);
    }
}
