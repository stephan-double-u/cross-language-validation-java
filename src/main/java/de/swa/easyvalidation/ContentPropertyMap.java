package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.Permissions;
import de.swa.easyvalidation.groups.ContentContraintGroup;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class ContentPropertyMap {
    // <property> -> {<permissions> -> ContentContraintGroup}
    private final Map<String, Map<Permissions, ContentContraintGroup>> map = new LinkedHashMap<>();

    public Set<String> getKeys() {
        return map.keySet();
    }

    public Collection<Map<Permissions, ContentContraintGroup>> getValues() {
        return map.values();
    }

    public Map<Permissions, ContentContraintGroup> getPermissionsMap(final Object property) {
        return map.get(property);
    }

    public Map<Permissions, ContentContraintGroup> putPermissionsMap(final String property, final Map<Permissions, ContentContraintGroup> permissionsMap) {
        return map.put(property, permissionsMap);
    }
}
