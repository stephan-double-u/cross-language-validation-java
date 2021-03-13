package de.swa.clv;

import de.swa.clv.constraints.Permissions;
import de.swa.clv.constraints.Conditions;
import de.swa.clv.json.JsonSerializable;

import java.util.LinkedHashMap;
import java.util.List;
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
    private final Map<Permissions, List<Conditions>> map = new LinkedHashMap<>();

    public Set<Permissions> keySet() {
        return map.keySet();
    }

    public List<Conditions> get(final Permissions permissions) {
        return map.get(permissions);
    }

    public Set<Map.Entry<Permissions, List<Conditions>>> entrySet() {
        return map.entrySet();
    }

    public void put(Permissions permissions, List<Conditions> contentConstraints) {
        map.put(permissions, contentConstraints);
    }

    @Override
    public String serializeToJson() {
        return map.entrySet().stream()
                .map(this::serialize)
                .collect(Collectors.joining(","));
    }

    // Special treatment here: sequence should be <contentConstraint>[,<permsissions>][,<constraintsTopGroup>]
    private String serialize(Map.Entry<Permissions, List<Conditions>> e) {
        final String contentConstraintJson = asKey("constraint") + asObject(e.getValue().get(0).getConstraint().serializeToJson());
        final String permissionsJson = e.getKey().serializeToJson();
        final String constraintsTopGroupJson = e.getValue().get(0).getConditionsTopGroup().serializeToJson();
        final String sep1 =  "".equals(permissionsJson) ? "" : ",";
        final String sep2 = "".equals(constraintsTopGroupJson) ? "" : ",";
        return asObject(contentConstraintJson + sep1 + permissionsJson + sep2 + constraintsTopGroupJson);
    }
}
