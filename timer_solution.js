// Update only this array for each new course run (one entry per course day).
// Always include the Berlin timezone offset: +02:00 (summer, CEST) or +01:00 (winter, CET)
const session_end_times = [
  new Date("2026-03-09T16:00+01:00"), // day 1
  new Date("2026-03-10T16:00+01:00"), // day 2
  new Date("2026-03-16T16:00+01:00"), // day 3
];

window.addEventListener("load", function () {
  const date_now = new Date();
  const show_solution = date_now >= session_end_times[course_day - 1];

  if (show_solution) {
    var x = document.getElementsByClassName("r_class_solution");
    for (let i = 0; i < x.length; i++) {
      x[i].style.display = "block";
    }
  }
});
