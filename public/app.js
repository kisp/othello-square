const form = document.querySelector("form");
form.addEventListener("submit", (event) => {
  event.preventDefault();
  const formData = new FormData(form);
  const nickname = formData.get("nickname");
  const message = document.querySelector("#message");
  message.textContent = `Welcome, ${nickname}!`;
  form.style.display = "none";
});
