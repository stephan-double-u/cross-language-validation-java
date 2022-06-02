package de.swa.clv;

import de.swa.clv.constraints.*;
import de.swa.clv.groups.ConditionsGroup;
import de.swa.clv.groups.ConditionsTopGroup;

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

public class ValidationTesting {

    public static void main(String[] args) {
        ValidationTesting validationTesting = new ValidationTesting();
        validationTesting.test();
    }
    public void test() {
        final ValidationRules<Reservation> rules = new ValidationRules<>(Reservation.class);

        rules.mandatory("customer",
                Condition.of("aBoolean", Equals.any(TRUE)));
        rules.mandatory("customer", Permissions.any("aaa"));
        rules.mandatory("id", Permissions.any("aaa"));
        rules.mandatory("id", Permissions.any("bbb"),
                ConditionsGroup.AND(
                        Condition.of("someString", Size.minMax(1, 3)),
                        Condition.of("articleArray", Size.min(2)),
                        Condition.of("someMap", Size.max(2))));
        rules.mandatory("id",
                ConditionsTopGroup.AND(
                        ConditionsGroup.OR(
                                Condition.of("id", Equals.none(1, 2, 3)),
                                Condition.of("id", Equals.none(4))),
                        ConditionsGroup.AND(Condition.of("id", Equals.any(1)))));

        rules.mandatory("articleList[0].name",
                Condition.of("articleArray[0].name", Equals.null_()));
        rules.mandatory("someInt",
                ConditionsGroup.AND(
                        Condition.of("someString", RegEx.any("nomatch", "N[A-Z]+")),
                        Condition.of("status", RegEx.any("E")),
                        Condition.of("startDate", Dates.future()),
                        Condition.of("startLocalDate", Dates.past(2)),
                        Condition.of("startCalDate", Dates.future(100))));

        rules.mandatory("status",
                ConditionsTopGroup.OR(
                        ConditionsGroup.AND(
                                Condition.of("someInt", Range.minMax(1, 999)),
                                Condition.of("someInt", Range.min(1).max(999)),
                                Condition.of("someLong", Range.max(Range.SAVE_INTEGER_MAX)),
                                Condition.of("aBoolean", Equals.any(TRUE)),
                                Condition.of("someInt", Equals.notNull()),
                                Condition.of("id", Equals.none(-1, 123456789))),
                        ConditionsGroup.AND(
                                Condition.of("id", Equals.none(666, 999)))));
        rules.mandatory("aBoolean",
                ConditionsTopGroup.AND(
                ConditionsGroup.OR(
                        Condition.of("someString", Size.minMax(1, 100)),
                        Condition.of("articleList", Size.min(1)),
                        Condition.of("articleArray", Size.max(100)) ),
                ConditionsGroup.OR(
                        Condition.of("id", Equals.none(404)))));
        rules.mandatory("customer.name",
                ConditionsGroup.AND(
                        Condition.of("status", Equals.any(ReservationStatus.NEW)),
                        Condition.of("status", Equals.notNull())
                ));

        final PropConstraint a = Condition.of("someString", Size.minMax(1, 100));

        rules.immutable("id");
        rules.immutable("status", Permissions.any(Perms.aaa), a);
        rules.immutable("status", a);
        rules.immutable("status");

        rules.content("status", Equals.any("one", "two"), Permissions.any(Perms.aaa));
        rules.content("status", Equals.any("NEW", "four"), Permissions.any("baz", "bar"));
        rules.content("status", Equals.any("five"), Permissions.any("..."), a);
        rules.content("status", Equals.any("five"));
        rules.content("stringList[0]", Equals.any("one", "two"));
        rules.content("someString", Equals.anyRef("articleList[0].name"));
        rules.content("id", Equals.any(101, 202, 303),
                ConditionsGroup.AND(a, a, a, a));
        //TODO List<ContentConstraints>!?: status == "foo" if other == 1 OR status == "bar" if other == 2

        // For validation of 'state-transitions' multiple rules per property are needed.
        // Rules are evaluated in definition sequence, first rules with matching permission, last rules w/o permissions
        // E.g. ONE -> [TWO, THREE], [TWO, THREE] -> FOUR
        // MANAGER is allowed to set any value
        rules.update("someEnum",
                Equals.any(SomeEnum.values()),
                Permissions.any("MANAGER"),
                Condition.of("someEnum",  Equals.any(ValidationTesting.SomeEnum.values())));
        rules.update("someEnum",
                Equals.any("ONE", "TWO", "THREE"),
                Condition.of("someEnum",  Equals.any("ONE")));
        rules.update("someEnum",
                Equals.any("TWO", "THREE", "FOUR"),
                Condition.of("someEnum",  Equals.any("TWO", "THREE")));
        rules.update("someEnum",
                Equals.any("FOUR"),
                Condition.of("someEnum",  Equals.any("FOUR")));
        // EXPERT is allowed to set FOUR (back to) ONE in addition to the default rules (i.e. w/o permissions)
        rules.update("someEnum",
                Equals.any("ONE", "FOUR"),
                Permissions.any("EXPERT"),
                Condition.of("someEnum",  Equals.any("FOUR")));

//        rules.content("id", Equals.any(101, 202, 303),
//                ConstraintsSubGroup.OR(a, a),
//                ConstraintsSubGroup.OR(a, a)
//                );
//        rules.content("id", Equals.any(101, 202, 303),
//                ConstraintsSubGroup.AND(a, a),
//                ConstraintsSubGroup.AND(a, a)
//                );
//        rules.content("id", Equals.any(101, 202, 303),
//                ConstraintsTopGroup.OR(
//                        ConstraintsSubGroup.AND(a, a),
//                        ConstraintsSubGroup.OR(a, a)
//                        )
//                );


        final ValidationRules<Article> articleRules = new ValidationRules<>(Article.class);
        articleRules.immutable("animalUse",
                ConditionsTopGroup.OR(
                        ConditionsGroup.AND(
                                Condition.of("animalUse", Equals.any(TRUE)),
                                Condition.of("usedOnce", Equals.any(TRUE))
                        ),
                        ConditionsGroup.AND(
                                Condition.of("medicalSetId", Equals.notNull())
                        )
                )
        );


        final List<Perms> ps = new ArrayList<Perms>() {{add(Perms.bar);}};

        final Reservation reservation1 = new Reservation(101, ReservationStatus.NEW, new Customer("Donald Duck"),
                Arrays.asList(new Article("Endoscope", true, true, null)));

        final List<String> err1 = Validator.instance().validateMandatoryRules(reservation1, rules);
        System.out.println("Validation errors: " + err1);

        final List<String> errx = Validator.instance().validateMandatoryRules(reservation1, UserPermissions.of(Perms.aaa, SomeEnum.ONE), rules);
        System.out.println("Validation errors: " + errx);

        final List<String> erry = Validator.instance().validateMandatoryRules(reservation1, UserPermissions.of(ps.toArray(new Perms[0])), rules);
        System.out.println("Validation errors: " + erry);

        final List<String> errz = Validator.instance().validateMandatoryRules(reservation1, UserPermissions.of("joe", "tom"), rules);
        System.out.println("Validation errors: " + errz);

        final Reservation reservation2 = new Reservation(101, ReservationStatus.APPROVED, new Customer("Donald Duck"),
                Arrays.asList(new Article("Endoscope", true, true, null)));

        final List<String> err2 = Validator.instance().validateImmutableRules(reservation1, reservation2, rules);
        System.out.println("Validation errors2: " + err2);

        final List<String> err3 = Validator.instance().validateContentRules(reservation1, rules);
        System.out.println("Validation errors: " + err3);

        final List<String> err4 = Validator.instance().validateContentRules(reservation1, UserPermissions.of(Perms.aaa, SomeEnum.ONE), rules);
        System.out.println("Validation errors: " + err4);

        final List<String> err5 = Validator.instance().validateContentRules(reservation1, rules);
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
                ConditionsGroup.AND(
                        Condition.of("status", Equals.any(ReservationStatus.NEW)),
                        Condition.of("someString", Equals.any("NEW")),
                        Condition.of("someInt", Equals.any(123)),
                        Condition.of("startLocalDate", Equals.any(LocalDate.now().minusDays(10))),
                        Condition.of("aBoolean", Equals.any(TRUE)),
                        Condition.of("status", Equals.anyRef("someString")),

                        Condition.of("status", Equals.none(ReservationStatus.RETURNED)),
                        Condition.of("someString", Equals.none("OLD")),
                        Condition.of("someInt", Equals.none(1, 2, 33)),
                        Condition.of("startLocalDate", Equals.none(LocalDate.now())),
                        Condition.of("aBoolean", Equals.none(FALSE)),
                        Condition.of("id", Equals.noneRef("someInt")),

                        Condition.of("nullString", Equals.null_()),
                        Condition.of("someString", Equals.notNull()),

                        Condition.of("someString", RegEx.any("nomatch", "N[A-Z]+")),

                        Condition.of("startDate", Dates.future()),
                        Condition.of("startLocalDate", Dates.past(2)),
                        Condition.of("startCalDate", Dates.future(100))
                ));
        System.out.println("serializeToJson: " + ValidationRules.serializeToJson(condAllInOne));
    }

    enum Perms {
        foo, bar, baz, aaa, xxx
    }

    enum SomeEnum {
        ONE, TWO, THREE, FOUR
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
        private final SomeEnum someEnum = SomeEnum.ONE;

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
        public SomeEnum getSomeEnum() { return someEnum; }
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
