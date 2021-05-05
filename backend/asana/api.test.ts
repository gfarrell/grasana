/* eslint-disable @typescript-eslint/camelcase */

import nock from "nock";
import {
  AsanaTask,
  AsanaResponse,
  getFromAsana,
  getTasksForProject,
} from "./api";

const ASANA_URL = "https://app.asana.com/api/1.0/";

describe("getFromAsana", () => {
  test("sends GET requests to the given path to Asana", async () => {
    const httpMock = nock(ASANA_URL).get("/my/path").reply(204);
    await getFromAsana("my/path");
    expect(httpMock.isDone()).toBe(true);
  });

  test("returns the data from the server", async () => {
    const expected: AsanaResponse = {
      data: {
        gid: "42",
        name: "My Amazing Task",
        resource_type: "task",
      },
    };
    nock(ASANA_URL).get("/path/with/data").reply(200, expected);
    const response = await getFromAsana("path/with/data");
    expect(response).toEqual(expected);
  });

  test("makes the request with the correct headers", async () => {
    const httpMock = nock(ASANA_URL, {
      reqheaders: {
        "authorization": "Bearer 12345",
        "accept": "application/json",
      },
    }).get("/hello").reply(204);
    await getFromAsana("hello");
    expect(httpMock.isDone()).toBe(true);
  });

  test("throws an error if it gets a non-2xx status", async () => {
    nock(ASANA_URL).get("/hello").reply(500);
    await expect(getFromAsana("hello"))
      .rejects.toThrow("HTTP 500: unable to retrieve hello from Asana");
  });
});

describe("getTasksForProject", () => {
  test("gets tasks for a given project from Asana", async () => {
    const data: AsanaTask[] = [
      { gid: "1", name: "Task A", resource_type: "task" },
      { gid: "2", name: "Task B", resource_type: "task" },
    ];
    nock(ASANA_URL).get("/projects/42/tasks").reply(200, { data });
    const response = await getTasksForProject("42");
    expect(response).toEqual(data);
  });
});
