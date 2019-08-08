package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.Permissions;
import de.swa.easyvalidation.groups.ContentContraints;
import de.swa.easyvalidation.json.JsonSerializable;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static de.swa.easyvalidation.json.JsonUtil.asKey;
import static de.swa.easyvalidation.json.JsonUtil.asObject;

/**
 * Maps {@code Permissions} to {@code ContentContraints}.
 */
public class ContentPermissionsMap implements JsonSerializable {
    // Linked HashMap to preserve insertion order ?!
    private final Map<Permissions, ContentContraints> map = new LinkedHashMap<>();

    public Set<Permissions> keySet() {
        return map.keySet();
    }

    public ContentContraints get(final Permissions permissions) {
        return map.get(permissions);
    }

    public Set<Map.Entry<Permissions, ContentContraints>> entrySet() {
        return map.entrySet();
    }

    public void put(Permissions permissions, ContentContraints contentContraints) {
        map.put(permissions, contentContraints);
    }

    @Override
    public String serializeToJson() {
        final String mapAsJson = map.entrySet().stream()
                .map(e -> serialize(e))
                .collect(Collectors.joining(","));
        return mapAsJson;
    }

    // Special treatment here: sequence should be <contentConstraint>[,<permsissions>][,<constraintsTopGroup>]
    private String serialize(Map.Entry<Permissions, ContentContraints> e) {
        final String contentConstraintJson = asKey("contentConstraint") + asObject(e.getValue().getContentConstraint().serializeToJson());
        final String permsissionsJson = e.getKey().serializeToJson();
        final String constraintsTopGroupJson = e.getValue().getConstraintsTopGroup().serializeToJson();
        final String sep1 = permsissionsJson == "" ? "" : ",";
        final String sep2 = constraintsTopGroupJson == "" ? "" : ",";
        return asObject(contentConstraintJson + sep1 + permsissionsJson + sep2 + constraintsTopGroupJson);
    }
}
