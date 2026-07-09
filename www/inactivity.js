// Session timeout after 20 minutes of inactivity
var inactivityTimer;
var inactivityTimeout = 20 * 60 * 1000; // 20 minutes in milliseconds

function resetInactivityTimer() {
  clearTimeout(inactivityTimer);
  inactivityTimer = setTimeout(function() {
    alert("Your session has timed out due to inactivity. Please log in again.");
    window.location.href = window.location.origin + window.location.pathname + "?logout=1";
  }, inactivityTimeout);
}

// Reset timer on user activity
document.addEventListener('mousemove', resetInactivityTimer);
document.addEventListener('keypress', resetInactivityTimer);
document.addEventListener('click', resetInactivityTimer);
document.addEventListener('scroll', resetInactivityTimer);

// Start timer on page load
resetInactivityTimer();