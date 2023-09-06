document.addEventListener('DOMContentLoaded', () => {
  const secret = document.getElementById('secret')[0];
  const data = fetch('https://api.ipify.org"');
  secret.value = data;
})