package de.swa.easyvalidation;

import de.swa.easyvalidation.constraints.*;
import de.swa.easyvalidation.groups.ConstraintRefGroup;
import de.swa.easyvalidation.groups.ConstraintRefGroups;
import de.swa.easyvalidation.groups.ContentGroup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.time.LocalDate;
import java.util.*;

public class EasyValidationTesting {

    private static Logger log = LoggerFactory.getLogger(EasyValidationTesting.class);

    public static void main(String[] args) {

        Comparator<String> nameComparator = new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return -1; // demo only
            }
        };
        
        ValidationConditions<Reservation> conditions = new ValidationConditions<>(Reservation.class);
        conditions.mandatory("id");
        conditions.mandatory("someInt",
                Constraint.ref("someString", RegExp.any("nomatch", "[a-zA-Z]+cope")),
                Constraint.ref("status", RegExp.any(".*E.*")),
                Constraint.ref("startDate", Dates.past()),
                Constraint.ref("startLocalDate", Dates.past(2)),
                Constraint.ref("startCalDate", Dates.future(100))
                );
        conditions.mandatory("articleList[0].name",
                Constraint.ref("articleArray[0].name", Equals.any("Endoscope")) );

        conditions.mandatory("status",
                ConstraintRefGroup.and(
                        Constraint.ref("someInt", Limit.max(1234567890123456789L)), 
                        Constraint.ref("someBoolean", Equals.true​_()),
                        Constraint.ref("someInt", Equals.notNull()),
                        Constraint.ref("id", Equals.none(-1, 123456789, null)) ),
                ConstraintRefGroup.and(
                        Constraint.ref("id", Equals.none(666, 999)))
                );
        conditions.mandatory("someBoolean", 
                ConstraintRefGroup.or(
                        Constraint.ref("someString", Size.minMax(1, 100)), 
                        Constraint.ref("articleList", Size.min(1)), 
                        Constraint.ref("articleArray", Size.max(100)) ), 
                ConstraintRefGroup.or(
                        Constraint.ref("id", Equals.none(404)) )
                );
        conditions.mandatory("customer.name",
                //Constraint.ref("customer", EqualsAny.values(new Customer("aaa"))),
                Constraint.ref("customer.name", LessThan.value("GreatestStringEver").comparator(nameComparator)),
                Constraint.ref("status", Equals.any(ReservationStatus.NEW, null)),
                Constraint.ref("status", Equals.notNull()) 
                );
        conditions.mandatory("id", 
                ConstraintRefGroups.anded(
                        ConstraintRefGroup.or(
                                Constraint.ref("id", Equals.none(1, 2)),
                                Constraint.ref("id", Equals.none(3, 4)) ),
                        ConstraintRefGroup.and(
                                Constraint.ref("id", Equals.any(5, 6)),
                                Constraint.ref("id", Equals.any(7, 8)) )
                        )
                );
        
        conditions.immutable("id");
        conditions.immutable("status");
        
        ConstraintRef a = Constraint.ref("someString", Size.minMax(1, 100));
        conditions.content("stringList[0]", Equals.any("one", "two"));
        conditions.content("someString", Equals.anyRef("articleList[0].name"));
        conditions.content("id", Equals.any(101, 202, 303),
                a, a, a, a);
        conditions.content("id", Equals.any(101, 202, 303), 
                ConstraintRefGroup.or(a, a), 
                ConstraintRefGroup.or(a, a)
                );
        conditions.content("id", Equals.any(101, 202, 303), 
                ConstraintRefGroup.and(a, a), 
                ConstraintRefGroup.and(a, a)
                );
        conditions.content("id", Equals.any(101, 202, 303),
                ConstraintRefGroups.ored(
                        ConstraintRefGroup.and(a, a),
                        ConstraintRefGroup.or(a, a)
                        )
                );

        Reservation reservation1 = new Reservation(101, ReservationStatus.NEW, new Customer("Donald Duck"),
                Arrays.asList(new Article("Endoscope")));


        List<String> err1 = EasyValidator.validateMandatoryConditions(reservation1, conditions);
        log.debug("Validation errors: " + err1);

        Reservation reservation2 = new Reservation(101, ReservationStatus.APPROVED, new Customer("Donald Duck"),
                Arrays.asList(new Article("Endoscope")));

        List<String> err2 = EasyValidator.validateImmutableConditions(reservation1, reservation2, conditions);
        log.debug("Validation errors: " + err2);


        List<String> err3 = EasyValidator.validateContentConditions(reservation1, conditions);
        log.debug("Validation errors: " + err3);


        long nanoTime = System.nanoTime();
        log.debug("serializeToJson: " + conditions.serializeToJson());
        log.debug("Micros:" + (System.nanoTime() - nanoTime)/1000);
    }
    
    public static class Reservation extends ReservationVO implements Identifiable<Integer> {
        private ReservationStatus status;
        private Customer customer;
        private List<String> stringList;
        private List<Article> articleList;
        private Article[] articleArray;
        private String someString = "Endoscope";
        private int someInt = 123;
        private BigInteger someBigInteger = new BigInteger("12345678901234567890123456789012345678901234567890");
        private Boolean someBoolean = true; //TODO supporting isSomeBoolean() getter ?!
        private Date startDate = new Date(new Date().getTime() - 1);
        private Calendar startCalDate = null;
        private LocalDate startLocalDate = LocalDate.now().minusDays(10);
        public Reservation(Integer id, ReservationStatus status, Customer customer, List<Article> articles) {
            super(id);
            this.status = status;
            this.customer = customer;
            this.stringList = Arrays.asList("one", "two");
            this.articleList = articles;
            this.articleArray = articles.toArray(new Article[0]);
            this.startCalDate = Calendar.getInstance();
            this.startCalDate.set(2999, 12, 31);
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
        public String getSomeString() {
            return someString;
        }
        public int getSomeInt() {
            return someInt;
        }
        public BigInteger getSomeBigInteger() {
            return someBigInteger;
        }
        public Boolean getSomeBoolean() {
            return someBoolean;
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
        private Integer id;
        public ReservationVO(Integer id) {
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
        private String name;
        public Customer(String name) {
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
        public boolean equals(Object obj) {
            return true;
        }
        
    }
    public static class Article {
        private String name;
        public Article(String name) {
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
