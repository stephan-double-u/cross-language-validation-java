package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.*;
import de.swa.easyvalidation.groups.RelationsSubGroup;
import de.swa.easyvalidation.groups.RelationsTopGroup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.net.URI;
import java.net.URL;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;

public class EasyValidationTesting {

    private static Logger log = LoggerFactory.getLogger(EasyValidationTesting.class);

    public static void main(String[] args) {

        final ValidationRules<Reservation> rules = new ValidationRules<>(Reservation.class);
        //rules.mandatory("customer", Constraint.ref("aBoolean", Equals.any("NEW")));
        rules.mandatory("customer", Constraint.ref("aBoolean", Equals.any(TRUE)));
        rules.mandatory("customer", Permissions.any("aaa"));
        rules.mandatory("id", Permissions.any("aaa"));
        rules.mandatory("id", Permissions.any("bbb"),
                Constraint.ref("someString", Size.minMax(1,3)),
                Constraint.ref("articleArray", Size.min(2)),
                Constraint.ref("someMap", Size.max(2)));
        rules.mandatory("id",
                RelationsTopGroup.AND(
                        RelationsSubGroup.OR(
                                Constraint.ref("id", Equals.none(1, 2, 3)),
                                Constraint.ref("id", Equals.none(4)) ),
                        RelationsSubGroup.AND(
                                Constraint.ref("id", Equals.any(1)) )
                )
        );

        rules.mandatory("articleList[0].name",
                Constraint.ref("articleArray[0].name", Equals.null_()));
        rules.mandatory("someInt",
                Constraint.ref("someString", RegEx.any("nomatch", "N[A-Z]+")),
                Constraint.ref("status", RegEx.any("E")),
                Constraint.ref("startDate", Dates.future()),
                Constraint.ref("startLocalDate", Dates.past(2)),
                Constraint.ref("startCalDate", Dates.future(100))
                );

        rules.mandatory("status",
                RelationsSubGroup.AND(
                        Constraint.ref("someInt", Range.minMax(1, 999)),
                        Constraint.ref("someInt", Range.min(1).max(999)),
                        Constraint.ref("someLong", Range.max(RangeRoot.SAVE_INTEGER_MAX)),
                        Constraint.ref("someInt", Range.minAny(1, 2).maxAny(8, 999)),
                        Constraint.ref("someInt", Range.maxAny(999)),
                        Constraint.ref("aBoolean", Equals.any(TRUE)),
                        Constraint.ref("someInt", Equals.notNull()),
                        Constraint.ref("id", Equals.none(-1, 123456789)) ),
                RelationsSubGroup.AND(
                        Constraint.ref("id", Equals.none(666, 999)))
                );
        rules.mandatory("aBoolean",
                RelationsSubGroup.OR(
                        Constraint.ref("someString", Size.minMax(1, 100)),
                        Constraint.ref("articleList", Size.min(1)),
                        Constraint.ref("articleArray", Size.max(100)) ),
                RelationsSubGroup.OR(
                        Constraint.ref("id", Equals.none(404)) )
                );
        rules.mandatory("customer.name",
                //Constraint.ref("customer", EqualsAny.values(new Customer("aaa"))),
                Constraint.ref("customer.name", Range.min("GreatestStringEver").maxAny("X", "Y").use(ComparisonType.LEXICOGRAPHICAL_UNICODE)),
                Constraint.ref("status", Equals.any(ReservationStatus.NEW)),
                Constraint.ref("status", Equals.notNull())
                );

        final PropConstraint a = Constraint.ref("someString", Size.minMax(1, 100));

        rules.immutable("id");
        rules.immutable("status", a);
        rules.immutable("status", Permissions.any(Perms.aaa), a);

        rules.content("status", Equals.any("one", "two"), Permissions.any(Perms.aaa));
        rules.content("status", Equals.any("NEW", "four"), Permissions.any("baz", "bar"));
        rules.content("status", Equals.any("five"));

        rules.content("stringList[0]", Equals.any("one", "two"));
        rules.content("someString", Equals.anyRef("articleList[0].name"));
        rules.content("id", Equals.any(101, 202, 303),
                a, a, a, a);

/*
        rules.content("id", Equals.any(101, 202, 303),
                RelationsSubGroup.or(a, a),
                RelationsSubGroup.or(a, a)
                );
        rules.content("id", Equals.any(101, 202, 303),
                RelationsSubGroup.and(a, a),
                RelationsSubGroup.and(a, a)
                );
        rules.content("id", Equals.any(101, 202, 303),
                RelationsTopGroup.ored(
                        RelationsSubGroup.and(a, a),
                        RelationsSubGroup.or(a, a)
                        )
                );
*/

        final ValidationRules<Article> articleRules = new ValidationRules<>(Article.class);
        articleRules.immutable("animalUse",
                RelationsTopGroup.OR(
                        RelationsSubGroup.AND(
                                Constraint.ref("animalUse", Equals.any(TRUE)),
                                Constraint.ref("usedOnce", Equals.any(TRUE))
                        ),
                        RelationsSubGroup.AND(
                                Constraint.ref("medicalSetId", Equals.notNull())
                        )
                )
        );


        final List<Perms> ps = new ArrayList<Perms>() {{add(Perms.bar);}};

        final Reservation reservation1 = new Reservation(101, ReservationStatus.NEW, new Customer("Donald Duck"),
                Arrays.asList(new Article("Endoscope", true, true, null)));

        final List<String> err1 = EasyValidator.instance().validateMandatoryRules(reservation1, rules);
        System.out.println("Validation errors: " + err1);

        final List<String> errx = EasyValidator.validateMandatoryRules(reservation1, UserPermissions.of(Perms.aaa, OtherEnum.dummy), rules);
        System.out.println("Validation errors: " + errx);

        final List<String> erry = EasyValidator.validateMandatoryRules(reservation1, UserPermissions.of(ps.toArray(new Perms[0])), rules);
        System.out.println("Validation errors: " + erry);

        final List<String> errz = EasyValidator.validateMandatoryRules(reservation1, UserPermissions.of("joe", "tom"), rules);
        System.out.println("Validation errors: " + errz);

        final Reservation reservation2 = new Reservation(101, ReservationStatus.APPROVED, new Customer("Donald Duck"),
                Arrays.asList(new Article("Endoscope", true, true, null)));

        final List<String> err2 = EasyValidator.validateImmutableRules(reservation1, reservation2, rules);
        System.out.println("Validation errors2: " + err2);

        final List<String> err3 = EasyValidator.validateContentRules(reservation1, rules);
        System.out.println("Validation errors: " + err3);

        final List<String> err4 = EasyValidator.validateContentRules(reservation1, UserPermissions.of(Perms.aaa, OtherEnum.dummy), rules);
        System.out.println("Validation errors: " + err4);

        final List<String> err5 = EasyValidator.validateContentRules(reservation1, rules);
        System.out.println("Validation errors: " + err5);


        final long nanoTime = System.nanoTime();
        System.out.println("serializeToJson: " + ValidationRules.serializeToJson(rules));
        System.out.println("Micros(!):" + (System.nanoTime() - nanoTime)/1000);

        ValidationRules<URL> cond1 = new ValidationRules<>(URL.class);
        ValidationRules<URI> cond2 = new ValidationRules<>(URI.class);
        System.out.println("serializeToJson: " + ValidationRules.serializeToJson(cond1, cond2));
        final ReservationValidationData reservationValidationData = ReservationValidationData.instance();
        System.out.println("serializeToJson: " + ValidationRules.serializeToJson(reservationValidationData));


        final ValidationRules<Reservation> condAllInOne = new ValidationRules<>(Reservation.class);
        condAllInOne.mandatory("id",
                Constraint.ref("status", Equals.any(ReservationStatus.NEW)),
                Constraint.ref("someString", Equals.any("NEW")),
                Constraint.ref("someInt", Equals.any(123)),
                Constraint.ref("startLocalDate", Equals.any(LocalDate.now().minusDays(10))),
                Constraint.ref("aBoolean", Equals.any(TRUE)),
                Constraint.ref("status", Equals.anyRef("someString")),

                Constraint.ref("status", Equals.none(ReservationStatus.RETURNED)),
                Constraint.ref("someString", Equals.none("OLD")),
                Constraint.ref("someInt", Equals.none(1d, 2L, 3f)),
                Constraint.ref("startLocalDate", Equals.none(LocalDate.now())),
                Constraint.ref("aBoolean", Equals.none(FALSE)),
                Constraint.ref("id", Equals.noneRef("someInt")),

                Constraint.ref("nullString", Equals.null_()),
                Constraint.ref("someString", Equals.notNull()),

                Constraint.ref("someString", RegEx.any("nomatch", "N[A-Z]+")),

                Constraint.ref("startDate", Dates.future()),
                Constraint.ref("startLocalDate", Dates.past(2)),
                Constraint.ref("startCalDate", Dates.future(100))
        );
        System.out.println("serializeToJson: " + ValidationRules.serializeToJson(condAllInOne));
    }

    enum Perms {
        foo, bar, baz, aaa, xxx
    }

    enum OtherEnum {
        dummy
    }

    class ReservationValidationRules {}
    public static class Reservation extends ReservationVO implements Identifiable<Integer> {
        private final ReservationStatus status;
        private final Customer customer;
        private final List<String> stringList;
        private final List<Article> articleList;
        private final Article[] articleArray;
        private final String someString = "NEW";
        private final String nullString = null;
        private final int someInt = 123;
        private final long someLong = 123456789L;
        private final BigInteger someBigInteger = new BigInteger("12345678901234567890123456789012345678901234567890");

        private final Boolean aBoolean = true; // Supporting isaBoolean() and getaBoolean getter!
        private final Date startDate = new Date(new Date().getTime() - 1);
        private Calendar startCalDate = null;
        private final LocalDate startLocalDate = LocalDate.now().minusDays(10);

        private final Map<String, String> someMap = new HashMap<String, String>() {{put("one", "1");put("two", "2");}};
        public Reservation(final Integer id, final ReservationStatus status, final Customer customer, final List<Article> articles) {
            super(id);
            this.status = status;
            this.customer = customer;
            stringList = Arrays.asList("one", "two");
            articleList = articles;
            articleArray = articles.toArray(new Article[0]);
            startCalDate = Calendar.getInstance();
            startCalDate.set(2999, 12, 31);
        }
        @Override
        public Integer getId() {
            return super.getId() * 2;
        }
        public ReservationStatus getStatus() {
            return status;
        }
        public Customer getCustomer() {
            return customer;
        }
        public List<Article> getArticleList() {
            return articleList;
        }
        public Article[] getArticleArray() {
            return articleArray;
        }
        public Map<String, String> getSomeMap() {
            return someMap;
        }
        public String getSomeString() {
            return someString;
        }
        public String getNullString() {
            return nullString;
        }
        public int getSomeInt() {
            return someInt;
        }
        public long getSomeLong() {
            return someLong;
        }
        public BigInteger getSomeBigInteger() {
            return someBigInteger;
        }
        public Boolean isaBoolean() {
            return aBoolean;
        }
        public Date getStartDate() {
            return startDate;
        }
        public LocalDate getStartLocalDate() {
            return startLocalDate;
        }
        public Calendar getStartCalDate() {
            return startCalDate;
        }

        public List<String> getStringList() {
            return stringList;
        }
    }
    public static class ReservationVO {
        private final Integer id;
        public ReservationVO(final Integer id) {
            super();
            this.id = id;
        }
        public Integer getId() {
            return id;
        }
    }
    public static interface Identifiable<T> {
        T getId();
    }
    public static class Customer {
        private final String name;
        public Customer(final String name) {
            this.name = name;
        }
        public String getName() {
            return name;
        }
        @Override
        public int hashCode() {
            return 31;
        }
        @Override
        public boolean equals(final Object obj) {
            return true;
        }

    }
    public static class Article {
        private final String name;
        private final boolean animalUse;
        private final boolean usedOnce;
        private final Long medicalSetId;
        public Article(final String name, boolean animalUse, boolean usedOnce, Long medicalSetId) {
            this.name = name;
            this.animalUse = animalUse;
            this.usedOnce = usedOnce;
            this.medicalSetId = medicalSetId;
        }
        public String getName() {
            return name;
        }
        public boolean isAnimalUse() {
            return animalUse;
        }
        public boolean isUsedOnce() {
            return usedOnce;
        }
        public Long getMedicalSetId() {
            return medicalSetId;
        }
    }
    public static enum ReservationStatus {
        NEW, APPROVED, DELIVERED, RETURNED
    }
    public static enum ArticleStatus {
        COMMISSIONING, NEW, ACTIVE, INACTIVE, DECOMMISSIONED
    }
}
