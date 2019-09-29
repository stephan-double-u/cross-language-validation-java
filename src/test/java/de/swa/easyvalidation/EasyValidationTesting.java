package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.Constraint;
import de.swa.easyvalidation.constraints.ConstraintRef;
import de.swa.easyvalidation.constraints.Dates;
import de.swa.easyvalidation.constraints.Equals;
import de.swa.easyvalidation.constraints.LessThan;
import de.swa.easyvalidation.constraints.LessThanString.ComparisonType;
import de.swa.easyvalidation.constraints.Limit;
import de.swa.easyvalidation.constraints.Permissions;
import de.swa.easyvalidation.constraints.RegEx;
import de.swa.easyvalidation.constraints.Size;
import de.swa.easyvalidation.groups.ConstraintsSubGroup;
import de.swa.easyvalidation.groups.ConstraintsTopGroup;
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

        final ValidationConditions<Reservation> conditions = new ValidationConditions<>(Reservation.class);
        //conditions.mandatory("customer", Constraint.ref("aBoolean", Equals.any("NEW")));
        conditions.mandatory("customer", Constraint.ref("aBoolean", Equals.any(TRUE)));
        conditions.mandatory("customer", Permissions.any("aaa"));
        conditions.mandatory("id", Permissions.any("aaa"));
        conditions.mandatory("id", Permissions.any("bbb"),
                Constraint.ref("someString", Size.minMax(1,3)),
                Constraint.ref("articleArray", Size.min(2)),
                Constraint.ref("someMap", Size.max(2)));
        conditions.mandatory("id",
                ConstraintsTopGroup.anded(
                        ConstraintsSubGroup.or(
                                Constraint.ref("id", Equals.none(1, 2, 3)),
                                Constraint.ref("id", Equals.none(4)) ),
                        ConstraintsSubGroup.and(
                                Constraint.ref("id", Equals.any(1)) )
                )
        );

        conditions.mandatory("articleList[0].name",
                Constraint.ref("articleArray[0].name", Equals.null_()));
        conditions.mandatory("someInt",
                Constraint.ref("someString", RegEx.any("nomatch", "N[A-Z]+")),
                Constraint.ref("status", RegEx.any("E")),
                Constraint.ref("startDate", Dates.future()),
                Constraint.ref("startLocalDate", Dates.past(2)),
                Constraint.ref("startCalDate", Dates.future(100))
                );

        conditions.mandatory("status",
                ConstraintsSubGroup.and(
                        Constraint.ref("someInt", Limit.max(Limit.SAVE_INTEGER_MAX)),
                        Constraint.ref("aBoolean", Equals.any(TRUE)),
                        Constraint.ref("someInt", Equals.notNull()),
                        Constraint.ref("id", Equals.none(-1, 123456789)) ),
                ConstraintsSubGroup.and(
                        Constraint.ref("id", Equals.none(666, 999)))
                );
        conditions.mandatory("aBoolean",
                ConstraintsSubGroup.or(
                        Constraint.ref("someString", Size.minMax(1, 100)),
                        Constraint.ref("articleList", Size.min(1)),
                        Constraint.ref("articleArray", Size.max(100)) ),
                ConstraintsSubGroup.or(
                        Constraint.ref("id", Equals.none(404)) )
                );
        conditions.mandatory("customer.name",
                //Constraint.ref("customer", EqualsAny.values(new Customer("aaa"))),
                Constraint.ref("customer.name", LessThan.lessThan("GreatestStringEver", ComparisonType.LEXICOGRAPHICAL_UNICODE)),
                Constraint.ref("status", Equals.any(ReservationStatus.NEW)),
                Constraint.ref("status", Equals.notNull())
                );

        final ConstraintRef a = Constraint.ref("someString", Size.minMax(1, 100));

        conditions.immutable("id");
        conditions.immutable("status", a);
        conditions.immutable("status", Permissions.any(Perms.aaa), a);

        conditions.content("status", Equals.any("one", "two"), Permissions.any(Perms.aaa));
        conditions.content("status", Equals.any("NEW", "four"), Permissions.any("baz", "bar"));
        conditions.content("status", Equals.any("five"));

        conditions.content("stringList[0]", Equals.any("one", "two"));
        conditions.content("someString", Equals.anyRef("articleList[0].name"));
        conditions.content("id", Equals.any(101, 202, 303),
                a, a, a, a);

/*
        conditions.content("id", Equals.any(101, 202, 303),
                ConstraintsSubGroup.or(a, a),
                ConstraintsSubGroup.or(a, a)
                );
        conditions.content("id", Equals.any(101, 202, 303),
                ConstraintsSubGroup.and(a, a),
                ConstraintsSubGroup.and(a, a)
                );
        conditions.content("id", Equals.any(101, 202, 303),
                ConstraintsTopGroup.ored(
                        ConstraintsSubGroup.and(a, a),
                        ConstraintsSubGroup.or(a, a)
                        )
                );
*/

        final List<Perms> ps = new ArrayList<Perms>() {{add(Perms.bar);}};

        final Reservation reservation1 = new Reservation(101, ReservationStatus.NEW, new Customer("Donald Duck"),
                Arrays.asList(new Article("Endoscope")));

        final List<String> err1 = EasyValidator.instance().validateMandatoryConditions(reservation1, conditions);
        System.out.println("Validation errors: " + err1);

        final List<String> errx = EasyValidator.validateMandatoryConditions(reservation1, UserPermissions.of(Perms.aaa, OtherEnum.dummy), conditions);
        System.out.println("Validation errors: " + errx);

        final List<String> erry = EasyValidator.validateMandatoryConditions(reservation1, UserPermissions.of(ps.toArray(new Perms[0])), conditions);
        System.out.println("Validation errors: " + erry);

        final List<String> errz = EasyValidator.validateMandatoryConditions(reservation1, UserPermissions.of("joe", "tom"), conditions);
        System.out.println("Validation errors: " + errz);

        final Reservation reservation2 = new Reservation(101, ReservationStatus.APPROVED, new Customer("Donald Duck"),
                Arrays.asList(new Article("Endoscope")));

        final List<String> err2 = EasyValidator.validateImmutableConditions(reservation1, reservation2, conditions);
        System.out.println("Validation errors2: " + err2);

        final List<String> err3 = EasyValidator.validateContentConditions(reservation1, conditions);
        System.out.println("Validation errors: " + err3);

        final List<String> err4 = EasyValidator.validateContentConditions(reservation1, UserPermissions.of(Perms.aaa, OtherEnum.dummy), conditions);
        System.out.println("Validation errors: " + err4);

        final List<String> err5 = EasyValidator.validateContentConditions(reservation1, conditions);
        System.out.println("Validation errors: " + err5);


        final long nanoTime = System.nanoTime();
        System.out.println("serializeToJson: " + ValidationConditions.serializeToJson(conditions));
        System.out.println("Micros(!):" + (System.nanoTime() - nanoTime)/1000);

        ValidationConditions<URL> cond1 = new ValidationConditions<>(URL.class);
        ValidationConditions<URI> cond2 = new ValidationConditions<>(URI.class);
        System.out.println("serializeToJson: " + ValidationConditions.serializeToJson(cond1, cond2));
        final ReservationValidationData reservationValidationData = ReservationValidationData.instance();
        System.out.println("serializeToJson: " + ValidationConditions.serializeToJson(reservationValidationData));


        final ValidationConditions<Reservation> condAllInOne = new ValidationConditions<>(Reservation.class);
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
        System.out.println("serializeToJson: " + ValidationConditions.serializeToJson(condAllInOne));
    }

    enum Perms {
        foo, bar, baz, aaa, xxx
    }

    enum OtherEnum {
        dummy
    }

    class ReservationValidationConditions {}
    public static class Reservation extends ReservationVO implements Identifiable<Integer> {
        private final ReservationStatus status;
        private final Customer customer;
        private final List<String> stringList;
        private final List<Article> articleList;
        private final Article[] articleArray;
        private final String someString = "NEW";
        private final String nullString = null;
        private final int someInt = 123;
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
        public Article(final String name) {
            this.name = name;
        }
        public String getName() {
            return name;
        }
    }
    public static enum ReservationStatus {
        NEW, APPROVED, DELIVERED, RETURNED
    }
    public static enum ArticleStatus {
        COMMISSIONING, NEW, ACTIVE, INACTIVE, DECOMMISSIONED
    }
}
