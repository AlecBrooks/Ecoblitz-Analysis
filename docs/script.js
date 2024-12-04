// Dynamically populate table with hyperlinks to all .html files in the same folder
document.addEventListener("DOMContentLoaded", () => {
    fetch(".")
        .then(response => response.text())
        .then(data => {
            const parser = new DOMParser();
            const doc = parser.parseFromString(data, "text/html");
            const links = doc.querySelectorAll("a[href$='.html']");
            
            const tableBody = document.querySelector("#files-table tbody");
            links.forEach(link => {
                if (link.href.endsWith("index.html")) return; // Skip the index.html file

                const fileName = link.getAttribute("href");
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
        .catch(error => console.error("Error fetching files:", error));
});