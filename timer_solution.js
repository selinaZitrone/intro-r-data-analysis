// please change it for a new course:
const date_course_start = new Date("2025-09-15");
const end_hour = 15; // course end at 15:00 --> 15, or 12:00 -> 12
    
// functions for calculating the date difference in weekdays
const isWeekday = date => date.getDay() % 6 !== 0;
const addDaysToDate = (date, n) => {
    const d = new Date(date);
    d.setDate(d.getDate() + n);
    return d;
};
const dateDifferenceInWeekdays = (startDate, endDate) =>
    Array.from({ length: (endDate - startDate) / 86_400_000 })
        .filter((_, i) => isWeekday(addDaysToDate(startDate, i + 1)))
        .length;
       
// calculation if solution should be shown
window.addEventListener("load", function() {
    const date_now = new Date();
    const days_since_start = 1 + dateDifferenceInWeekdays(date_course_start, date_now);
    const hours_today = date_now.getHours();

    const show_solution = 
        days_since_start > course_day || 
        (days_since_start == course_day && hours_today >= end_hour);

    if (show_solution) {
        var x = document.getElementsByClassName("r_class_solution");
        for (let i = 0; i < x.length; i++) {
            x[i].style.display = "block";
        }
    }
});