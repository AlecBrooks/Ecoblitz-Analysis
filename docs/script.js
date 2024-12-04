document.addEventListener("DOMContentLoaded", () => {
    fetch("files.json")
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            return response.json();
        })
        .then(files => {
            const tableBody = document.querySelector("#files-table tbody");
            files.forEach(fileName => {
                const row = document.createElement("tr");
                const cell = document.createElement("td");
                const anchor = document.createElement("a");

                anchor.href = fileName;
                anchor.textContent = fileName;

                cell.appendChild(anchor);
                row.appendChild(cell);
                tableBody.appendChild(row);
            });
        })
        .catch(error => console.error("Error fetching file list:", error));
});